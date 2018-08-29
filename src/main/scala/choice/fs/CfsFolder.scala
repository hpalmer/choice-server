/**
  * Copyright © 2012-2016 The Board of Trustees of The Leland Stanford Junior University.
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

import choice.access._
import choice.actor.DbManager
import choice.fs.vfs.{VFile, Vnode}
import choice.model.Resource
import net.liftweb.common._
import net.liftweb.mapper.BaseMetaMapper

import scala.annotation.tailrec

/**
 * This is a CfsVnode for a generic folder. A folder can contain members of types
 * other than "choice/metadata". This defines a folder which can contain members
 * of any MIME types, not forbidden by the parent and child restrictions in the
 * CfsVnode companion.
 *
 * @param resource the resource id of the DB Resource entry for this folder
 */
class CfsFnode(resource : Resource) extends CfsSpecialNode(resource) {
    /**
     * Create a file handle (VFile or its subclass) for a resource of this MIME type.
     *
     * @param path the filename path used to locate the file
     * @param principal the principal responsible for this open operation
     * @param options options that may affect the open operation
     * @return a boxed CfsFolder. A Failure may be returned for various errors, such as
     *         insufficient access rights, or unsupported options.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[CfsFolder] = {
        Full(new CfsFolder(path, principal, this))
    }

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = true
}

/**
 * This is a CfsFile file handle for a generic folder.
 *
 * @param path the path used to locate the file
 * @param principal the principal responsible for file operations performed using
 *                  this handle
 * @param vnode the CfsFnode of the file referenced by this handle
 */
class CfsFolder(path : CfsAbsolutePath, principal : Principal, vnode : CfsFnode)
    extends CfsSpecial(path, principal, vnode) with CfsDirFile {

    import CfsFolder.Log

    override def getVnode: CfsFnode = vnode

    /**
     * Get a list of all the non-special file resources in this folder.
     */
    def getPlainFiles: List[VFile] = CfsFolder.canListMember(this) { () ⇒
        vnode.withReadLock { () ⇒ DbManager lookupMembers vnode } match {
            case Full(list) ⇒
                val outlist = filterMembers(list, Nil) { (name, vn) ⇒
                    vn match {
                        case cfsvn: CfsVnode ⇒
                            if (cfsvn.isSpecial_?) {
                                cfsvn.release
                                None
                            }
                            else (cfsvn open(path / name, principal, CfsOpenOptions.Default)).toOption
                        case regvn: Vnode ⇒
                            if (regvn.isContainer_?) {
                                regvn.release
                                None
                            }
                            else (regvn open(path / name, principal, CfsOpenOptions.Default)).toOption
                    }
                }
                Full(outlist)
            case e : EmptyBox ⇒ e
        }
    } openOr Nil

    /**
     * Get a list of the sub-folders of this folder.
     */
    def getFolders: List[CfsFolder] = CfsFolder.canListMember(this) { () ⇒
        vnode.withReadLock { () ⇒ DbManager lookupMembers vnode } match {
            case Full(list) ⇒
                val result = filterMembers[CfsFolder](list, Nil) { (name, vn) ⇒
                    vn match {
                        case cfsfvn: CfsFnode ⇒
                            assert(cfsfvn.isReferenced_?)
                            // open() releases the Vnode in the event of an error
                            cfsfvn open(path / name, principal, CfsOpenOptions.Default) match {
                                case Full(cfsfolder: CfsFolder) ⇒ Some(cfsfolder)
                                case Full(other) ⇒
                                    Log.error(s"CfsNode did not open as a CfsFolder [${cfsfvn.getResourceId}]")
                                    other close ()
                                    None
                                case _ : EmptyBox ⇒ None
                            }
                        case notFolder ⇒
                            assert(notFolder.isReferenced_?)
                            notFolder.release
                            None
                    }
                }
                Full(result)
            case e: EmptyBox ⇒ e
        }
    } openOr Nil

    /**
     * Get a list of plain files and a list of sub-folders.
     */
    def getFoldersAndFiles : (List[CfsFolder], List[VFile]) = CfsFolder.canListMember (this) { () ⇒
        def helper(list : List[VFile], acc : (List[CfsFolder], List[VFile])) : (List[CfsFolder], List[VFile]) = {
            list match {
                case Nil ⇒ acc
                case head :: tail ⇒
                    head match {
                        case folder : CfsFolder ⇒ helper(tail, (folder :: acc._1, acc._2))
                        case vfile ⇒ helper(tail, (acc._1, vfile :: acc._2))
                    }
            }
        }
        vnode.withReadLock { () ⇒ DbManager lookupMembers vnode } match {
            case Full(list) ⇒
                val allmembers = filterMembers[VFile] (list, Nil) { (name, vn) ⇒
                    (vn open (path / name, principal, CfsOpenOptions.Default)).toOption
                }
                Full(helper (allmembers, (Nil, Nil)))
            case e : EmptyBox ⇒ e
        }
    } openOr ((Nil, Nil))

    /**
     */

    /**
     * Create a child folder in this folder. The child will have the same MIME type as
     * the parent.
     *
     * @param name the name of the child folder
     * @param options create options for the child folder
     * @return a boxed file handle for the child folder
     */
    def makeFolder(name : String, options : CfsCreateOptions = CfsCreateOptions.Default) : Box[CfsFolder] = {
        CfsFolder.canAddMember (this) { () ⇒
            // Make the child folder the same MIME type as the parent, if this is called
            // from a subclass of CfsFolder.
            create(name, getVnode.getMimeType, options) match {
                case Full(mfolder : CfsFolder) ⇒ Full(mfolder)
                case Full(other) ⇒
                    unlinkUnchecked(other, recursive = false)
                    other close ()
                    Failure(s"'${path / name}' did not open as a folder")
                case e : EmptyBox ⇒ e
            }
        }
    }

    /**
     * Given a list of tuples containing member names and vnodes, apply
     * a specified function to obtain an optional value for each member.
     *
     * The member Vnodes passed to the function f have been acquired, and
     * the function is responsible for releasing them, or using them to
     * open a file that it returns.
     *
     * @param names a list of member tuples of this folder
     * @param acc an accumulated list of values produced
     * @param f a function that produces an optional value for each member
     * @tparam T the type of value produced by f
     * @return a list of values produced by f
     */
    @tailrec
    protected final def filterMembers[T](names : List[(String, CfsVnode)], acc : List[T])
                                        (f : (String, Vnode) ⇒ Option[T]) : List[T] = {
        names match {
            case Nil ⇒ acc
            case (name, mvnode) :: tail ⇒
                f(name, mvnode) match {
                    case Some(t) ⇒ filterMembers(tail, t :: acc)(f)
                    case None ⇒ filterMembers(tail, acc)(f)
                }
        }
    }
}

trait CfsFolderHandler[N <: CfsFnode, F <: CfsFolder] extends ContainerMimeType {

    def name : String
    def makeNode(resource : Resource) : Box[N]
    //def makeFile(path : VPath, principal : Principal, vnode : N) : Box[F]
    def rights : List[RightDef]

    override def isContainer_? = true

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override def getRights : List[RightDef] = rights

    /**
     * Construct a folder Vnode from a DB Resource object.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    override def instantiate(resource : Resource) : Box[N] = {
        if (isType_?(resource)) makeNode (resource)
        else Failure(s"failed to instantiate resource id ${resource.getSafeKey} as a $name Vnode")
    }

}

object CfsFolder extends CfsFolderHandler[CfsFnode, CfsFolder] {

    override protected val Log = Logger("choice.model.Folder")

    override val getName = "File Folder Type"

    override val getSchemas : List[BaseMetaMapper] = Nil

    override val getMimeType : String = "choice/folder"

    override val name = "folder"

    override def makeNode(resource : Resource) : Box[CfsFnode] = Full(new CfsFnode(resource))

    /**
     * Open the root folder, checking that the specified principal can at least list
     * its contents.
     *
     * @param principal the principal for this operation, and for the returned file handle
     * @return a boxed CfsFolder handle for the root if successful, otherwise Failure
     */
    def openRootFolder(principal : Principal) : Box[CfsFolder] = {
        val rvnode = Cfs.getRoot
        assert(rvnode.isReferenced_?)
        rvnode open (CfsRootPath, principal, CfsOpenOptions.Default) match {
            case Full(root : CfsFolder) ⇒ CfsFolder.canListMember (root) { () ⇒ Full(root) }
            case Full(other) ⇒
                val msg = s"CfsRootPath did not open as a CfsFolder [${rvnode.getResourceId}]"
                Log.error(msg)
                other close ()
                Failure(msg)
            case e : EmptyBox ⇒ e
        }
    }

    val rights = List(
        RightDef("create_folder", "file folder", "create a folder"),
        RightDef("unlink_folder", "file folder", "unlink a folder"),
        RightDef("list_folder", "file folder", "lookup and list members, read metadata"),
        RightDef("add_folder_member", "file folder", "create a new member file"),
        RightDef("replace_folder_member", "file folder", "replace an existing member file"),
        RightDef("remove_folder_member", "file folder", "remove a member file"),
        RightDef("write_folder_meta", "file folder", "write folder metadata")
    )

    /**
     * This defines the rights needed to list members of the container, or to reference
     * them by name.
     *
     * @return a RightsCheck instance
     */
    val canListMember : RightsCheck = AnyRightsCheck("list_folder",
                                                     "add_folder_member",
                                                     "replace_folder_member",
                                                     "remove_folder_member")

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    val canAddMember : RightsCheck = AnyRightsCheck("add_folder_member")

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    val canRemoveMember = AnyRightsCheck("remove_folder_member")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_folder")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_folder")
}
