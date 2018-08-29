/**
  * Copyright © 2014-2017 The Board of Trustees of The Leland Stanford Junior University.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
/**
  * @author Howard Palmer
  */
package choice.fs

import net.liftweb.common._
import choice.actor.DbManager
import choice.access.Principal
import choice.fs.vfs.{VInfo, VDirFile, VFile}

/**
 * This trait adds a container interface to a CfsFile.
 */
trait CfsDirFile extends CfsFile with VDirFile { self : CfsFile ⇒

    /**
     * Create a new member of this container, with a specified name and MIME type.
     * The options may be specific to the type of file being created. The owner of
     * the new file will be the principal associated with this container handle.
     *
     * The principal associated with this container must have an access right to
     * create a file of the specified MIME type, and must have the right to add
     * a member to this container.
     *
     * @param member the name for the new member, which must not already exist,
     *               unless the filesystem implements versioning
     * @param mimeType the MIME type associated with the new file
     * @param options options that may affect the create
     * @return a boxed VFile if the file is created successfully, or a
     *         Failure otherwise. Empty should not be returned.
     */
    def create(member : String, mimeType : String, options : CfsCreateOptions) : Box[CfsFile] = {
        // First check that the principal can create objects of the indicated
        // MIME type in this container.
        DbManager getMimeTypeHandler mimeType match {
            case Full(mth) ⇒ mth.canCreateObject (this) { () ⇒
                getMimeTypeHandler match {
                    case Full(cmt : ContainerMimeType) ⇒
                        // Now check that the principal can add a member to this container
                        cmt.canAddMember (this) (() ⇒ createUnchecked(member, mimeType, options))
                    case Full(_) ⇒ Failure(s"create: $getPath is not a container")
                    case Empty ⇒ Failure(s"create: no handler for $mimeType")
                    case f : Failure ⇒ f
                }
            }
            case Empty ⇒ Failure(s"create: no handler for $mimeType")
            case f : Failure ⇒ f
        }
    }

    /**
     * Link a given file as a member of this container, under a specified name.
     * The specified member file handle is not closed, regardless of the
     * success or failure of the operation.
     *
     * @param name the name to be given the member in this container
     * @param member the file to be linked as a member
     * @return a boxed VFile of the linked member file, with its path referencing
     *         the newly created link. Failure on error.
     */
    def link(name : String, member : VFile) : Box[VFile] = getMimeTypeHandler match {
        case Full(mth : ContainerMimeType) ⇒ mth.canAddMember (this) (() ⇒ linkUnchecked(name, member))
        case Full(_) ⇒ Failure(s"$getPath is not a container")
        case Empty ⇒ Failure(s"$getPath: no handler for MIME type")
        case f : Failure ⇒ f
    }

    def linkUnchecked(name : String, member : VFile) : Box[VFile] = {
        member match {
            case cfsmem : CfsFile ⇒
                getVnode.withWriteLock { () ⇒
                    val memvnode = cfsmem.getVnode
                    // This acquire is for the open() below
                    memvnode.acquire
                    memvnode.withWriteLock { () ⇒
                        DbManager reference memvnode flatMap { _ ⇒
                            getVnode link (name, memvnode) match {
                                case Full(memberName) ⇒
                                    memvnode open (getPath / memberName, getPrincipal, CfsOpenOptions.Default) match {
                                        case full @ Full(_) ⇒ full
                                        case e : EmptyBox ⇒
                                            // The link() worked, so the file is there, but this principal
                                            // is unable to open it. This could happen with some path-based
                                            // access control.
                                            memvnode.release
                                            e
                                    }
                                case e : EmptyBox ⇒
                                    // The link() did not work, so undo the reference() we did above.
                                    // Also undo the acquire, since there's nothing to open().
                                    DbManager dereference memvnode
                                    memvnode.release
                                    e
                            }
                        }
                    }
                }
            case _ ⇒
                Failure(s"linking to non-Cfs files is currently unsupported")
        }
    }

    /**
     * Unlink a member from this container. If this is the last link to the member,
     * it is deleted from the filesystem, and a recursive unlink is performed on its
     * metadata (if metadata is supported). If the member is itself a container, it
     * must be empty (except for metadata), or else the recursive option must be
     * true. A recursive unlink may be partially completed before returning an error.
     *
     * @param member the name of the member to be unlinked from this container
     * @param recursive if the member is a container, recursively unlink its members
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container, or the principal lacked
     *         the proper access rights, or on any other error.
     */
    def unlink(member : String, recursive : Boolean) : Box[Boolean] = {
        getMimeTypeHandler match {
            case Full(mth : ContainerMimeType) ⇒ mth.canRemoveMember (this) { () ⇒
                getMember (member) match {
                    case Full(cfsfile : CfsFile) ⇒
                        // The member being removed can also specify a right that the
                        // principal must have for the container, in order to unlink
                        // objects of its specific type from the container.
                        val mimeType = cfsfile.getVnode.getMimeType
                        val result = DbManager getMimeTypeHandler mimeType match {
                            case Full(mmth) ⇒ mmth.canUnlinkObject (this) { () ⇒
                                unlinkUnchecked(cfsfile, recursive)
                            }
                            case Empty ⇒ Failure(s"unlink: no handler for $mimeType")
                            case f : Failure ⇒ f
                        }
                        cfsfile close ()
                        result
                    case Full(other) ⇒
                        other close ()
                        Failure(s"unlink: non-Cfs file $member")
                    case Empty ⇒ Failure(s"$member does not exist")
                    case f : Failure ⇒ f
                }
            }
            case Full(_) ⇒ Failure(s"$getPath is not a container")
            case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
            case f : Failure ⇒ f
        }
    }

    def unlinkUnchecked(memberfile : VFile, recursive : Boolean) : Box[Boolean] = {
        val membername = memberfile.getName
        (memberfile match {
            case memvfile : CfsFile ⇒
                val cvnode = getVnode
                val memvnode = memvfile.getVnode
                // Get write locks on the container and the member
                cvnode.withWriteLock { () ⇒
                    memvnode.withWriteLock { () ⇒
                        def helper(list : List[String], err : EmptyBox) : Box[Boolean] = {
                            list match {
                                case Nil ⇒ if (err == Empty) Full(true) else err
                                case head :: tail ⇒
                                    // Members can be unlinked if recursive is specified, or if the
                                    // only member is the metadata folder.
                                    if (recursive) {
                                        memvfile match {
                                            case memdirfile : CfsDirFile ⇒
                                                val nexterr = memdirfile unlink (head, recursive) match {
                                                    case Full(_) ⇒ err
                                                    case Empty ⇒ Failure(s"unlink $head returned Empty", Empty, err)
                                                    case f : Failure ⇒ new Failure(f.msg, f.exception, err)
                                                }
                                                helper(tail, nexterr)
                                            case _ ⇒
                                                // TODO: head could still be the metadata folder here
                                                Failure(s"recursive unlink invoked on CfsFile")
                                        }
                                    }
                                    else Failure(s"'$membername' is not empty and recursive unlink was not specified")
                            }
                        }
                        cvnode unlink (membername, memvnode) flatMap { pair ⇒
                            val (result, list) = pair
                            if (list == Nil) Full(result)
                            else {
                                // Unlink was not done. Instead we got a list of members of the member
                                // we're trying to unlink. These members must be unlinked first.
                                helper(list, Empty) flatMap { _ ⇒
                                    // Once the members have been removed, try the unlink again. If there
                                    // are still members, it fails.
                                    cvnode unlink (membername, memvnode) match {
                                        case Full((b, Nil)) ⇒ Full(b)
                                        case Full(_) ⇒ Failure(s"unlink '$membername' appears incomplete")
                                        case e : EmptyBox ⇒ e
                                    }
                                }
                            }
                        }
                    }
                }
            case _ ⇒ Failure(s"cannot unlink non-Cfs file '$membername")
        }) ?~ s"${getPath.toString} does not contain $membername"
    }

    /**
     * Check whether this container contains a member with a specified name.
     *
     * @param member the name of the potential member
     * @return true if the principal can ascertain the existence of the member,
     *         false otherwise
     */
    def exists_?(member : String) : Box[Boolean] = getMimeTypeHandler match {
        case Full(mth : ContainerMimeType) ⇒ mth.canListMember (this) { () ⇒
            Full(getVnode exists_? member)
        }
        case Full(_) ⇒ Failure(s"$getPath is not a container")
        case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
        case f : Failure ⇒ f
    }

    /**
     * Get the names of all the members in this container. Optionally, only
     * members of a given MIME type can be returned.
     *
     * @param mimeType an optional MIME type to select only members of this type
     * @return the boxed member names, with an empty sequence indicating there are none.
     *         Failure on error.
     */
    def getMembers(mimeType : Option[String]) : Box[Seq[String]] = getMimeTypeHandler match {
        case Full(mth : ContainerMimeType) ⇒ mth.canListMember (this) { () ⇒
            getVnode getMembers mimeType
        }
        case Full(_) ⇒ Failure(s"getMembers: $getPath is not a container")
        case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
        case f : Failure ⇒ f
    }

    /**
     * Check whether this container is empty.
     *
     * @return true if the container is empty, false if not.
     */
    def isEmpty_? : Box[Boolean] = getMimeTypeHandler match {
        case Full(mth : ContainerMimeType) ⇒ mth.canListMember (this) { () ⇒ Full(getVnode.isEmpty_?) }
        case Full(_) ⇒ Failure(s"isEmpty_?: $getPath is not a container")
        case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
        case f : Failure ⇒ f
    }

    /**
     * Retrieve information about a member of this container with a specified
     * name. Some filesystems may support options affecting the information
     * returned.
     *
     * @param member the name of member
     * @param options optional options for operation
     * @return a VInfo instance
     */
    def info(member : String, options : Option[Map[String, Any]]) : Box[VInfo] = getMimeTypeHandler match {
        case Full(mth : ContainerMimeType) ⇒ mth.canListMember (this) { () ⇒
            open (member, getPrincipal, CfsOpenOptions.Default) flatMap { vfile ⇒
                val result = vfile info options
                vfile close ()
                result
            }
        }
        case Full(_) ⇒ Failure(s"info: $getPath is not a container")
        case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
        case f : Failure ⇒ f
    }

    /**
     * Open a specified member of this container. This permits a different principal
     * to be used than was used to open this container. The supported options will
     * generally be the same as if the member were opened by its full filename path.
     *
     * @param member the member name within the container
     * @param principal the principal responsible for this operation
     * @param options options which may affect how the file is opened
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    def open(member : String, principal : Principal, options : CfsOpenOptions) : Box[VFile] = {
        getMimeTypeHandler match {
            case Full(mth : ContainerMimeType) ⇒ mth.canListMember (this) { () ⇒
                openUnchecked(member, principal, options)
            }
            case Full(_) ⇒ Failure(s"open: $getPath is not a container")
            case Empty ⇒ Failure(s"no handler for MIME type ${getVnode.getMimeType}")
            case f : Failure ⇒ f
        }
    }

    /**
     * Alternate form of open() which uses the principal that was used to open the
     * container.
     *
     * @param member the member name within the container
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    def getMember(member : String) : Box[VFile] = open (member, getPrincipal, CfsOpenOptions.Default)

    def withMember[T](member : String)(f : Cfs.FilePF[T]) : Box[T] = {
        getMember (member) match {
            case Full(cfsfile : CfsFile) ⇒
                val result =
                    if (f.isDefinedAt(cfsfile)) f(cfsfile)
                    else Failure(s"$member is the wrong type")
                cfsfile close ()
                result
            case Full(vfile) ⇒
                vfile close ()
                Failure(s"$member is not a Cfs file")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Open a specified member of this container. This permits a different principal
     * to be used than was used to open this container. The supported options will
     * generally be the same as if the member were opened by its full filename path.
     *
     * @param member the member name within the container
     * @param principal the principal responsible for this operation
     * @param options options which may affect how the file is opened
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    protected def openUnchecked(member : String, principal : Principal,
                                options : CfsOpenOptions) : Box[VFile] = {
        Cfs.withValidFilename (member) { _ ⇒
            getVnode lookup (member, keepParent = true) flatMap { cvnode ⇒
                cvnode open (getPath / member, principal, options)
            }
        }
    }

    /**
     * Create a member in this container.
     *
     * @param member the member name
     * @param mimeType the MIME type of the new member
     * @param options optional options
     * @return the boxed member file if it is created successfully, otherwise Failure
     */
    protected def createUnchecked(member : String, mimeType : String, options : CfsCreateOptions) : Box[CfsFile] = {
        Cfs.withValidFilename (member) { _ ⇒
            DbManager createFile (getPrincipal, mimeType, options) flatMap { newvnode ⇒
                assert(newvnode.isReferenced_?)
                newvnode cfsCreate (member, getPrincipal, options) match {
                    case Full(mvnode) ⇒
                        getVnode link (member, mvnode) match {
                            case Full(memberName) ⇒
                                mvnode open (getPath / memberName, getPrincipal, CfsOpenOptions.Default) match {
                                    case Full(memfile) ⇒ Full(memfile)
                                    case e : EmptyBox ⇒
                                        // The file was created, but couldn't be opened.
                                        // The reference count on the vnode should have been decremented
                                        // by open() as a result of the error.
                                        assert(!mvnode.isReferenced_?)
                                        // A principal who creates a file should be able to open it as the owner.
                                        // If the open fails, the file should not continue to exist.
                                        this unlink (member, recursive = false)
                                        e
                                }
                            case e : EmptyBox ⇒
                                newvnode.withWriteLock { () ⇒ DbManager dereference newvnode }
                                newvnode.release
                                assert(!newvnode.isReferenced_?)
                                e
                        }
                    case e : EmptyBox ⇒
                        newvnode.withWriteLock { () ⇒ DbManager dereference newvnode }
                        newvnode.release
                        assert(!newvnode.isReferenced_?)
                        e
                }
            }
        }
    }

    /**
     * Retrieve information about a member of this container with a specified
     * name. Some filesystems may support options affecting the information
     * returned.
     *
     * @param member the name of member
     * @param options optional options for operation
     * @return a VInfo instance
     */
    protected def infoMemberUnchecked(member : String, options : Option[Map[String, Any]]) : Box[VInfo] = {
        getMember (member) flatMap { vfile ⇒
            val result = vfile info options
            vfile close()
            result
        }
    }
}
