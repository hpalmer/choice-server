/**
  * Copyright © 2014-2016 The Board of Trustees of The Leland Stanford Junior University.
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

import java.util.concurrent.atomic.AtomicBoolean

import choice.access._
import choice.actor.DbManager
import choice.attributes.{AttrDef, AttrVal}
import choice.fs.vfs.AttributeType.AttributeType
import choice.fs.vfs._
import choice.lib.ExtendedBox.extendBox
import choice.model.{GlobalConfig, ResourceId}
import net.liftweb.common._
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers._

/**
 * Basic file handle for a Cfs file. Specific file types may use subclasses of this.
 *
 * @param path the path used to locate the file
 * @param principal the principal responsible for file operations performed using
 *                  this handle
 * @param vnode the Vnode of the file referenced by this handle
 */
abstract class CfsFile(path : CfsAbsolutePath, principal : Principal, vnode : CfsVnode) extends VFile
                                                                                                with VfsMeta
                                                                                                with CanHazMap {
    import choice.fs.Cfs.Log

    /** List of PathPolicy entries that apply to this file */
    protected lazy val pathPolicies : List[PathPolicy] = PathPolicy getEntriesForFile this

    /**
     * Find all the rights a given principal has for this file, including both resource
     * rights and path-based rights.
     *
     * @param principal the principal whose rights are required
     * @return a list of rights as right ids
     */
    protected def getRightsOf(principal : Principal) : List[RightId] = {
        val resrights = getVnode getRights principal
        val pathrights = pathPolicies flatMap { pp ⇒
            Cfs.withPolicyNode (ResourceId(pp.policy.get)) { pnode ⇒
                Full(pnode getRightsOf principal)
            } openOr Nil
        }
        (resrights union pathrights).distinct
    }

    /**
     * Find all the access rights that the principal associated with this file handle
     * holds for the associated file. This is evaluated at most once during the
     * lifetime of the file handle, and therefore will not reflect any changes to
     * access rights after that evaluation.
     */
    protected lazy val rights : List[RightId] = getRightsOf(getPrincipal)

    /** Get the Vnode associated with this file handle */
    override def getVnode : CfsVnode = ifNotClosed(() ⇒ vnode)

    /** Get the principal associated with this file handle */
    override def getPrincipal : Principal = principal

    /**
     * Determine if a given principal holds any of a specified list of rights for this file.
     * Policies may be set directly on the resource associated with the file, so that they
     * apply regardless of the path by which the file is accessed (assuming the principal
     * has the rights needed to traverse the path). Policies may also be applied to file paths,
     * and will be included whenever the file path matches the pattern in the path-based setting.
     *
     * The owner of the file has all rights, as does the BootPrincipal, the SystemPrincipal,
     * or any system admin principal. If the owner is a group, all descendant users of the
     * group are considered owners.
     *
     * @param rightIds a list of access right ids
     * @param principal the principal whose rights are to be determined
     * @return true if the principal has any of the rights
     */
    def hasAnyRights(rightIds : ⇒ Seq[RightId], principal : Principal) : Boolean = {
        val pid = principal.getPrincipalId
        val ok = (pid == getVnode.getOwnerId) || (principal eq BootPrincipal) || (principal eq SystemPrincipal) || {
            val prights = if (pid == getPrincipalId) rights else getRightsOf(principal)
            prights exists (fileRight ⇒ rightIds contains fileRight)
        } || principal.isSystemAdmin_? || {
            // Last chance. The owner might be a group to which the principal belongs.
            val ancestors = principal.getAncestors
            val ownerId = getVnode.getOwnerId
            ancestors exists (_.getPrincipalId == ownerId)
        }
        ok
    }

    /**
     * Determine if a given principal holds all of the rights in a specified list, for this file.
     * Policies may be set directly on the resource associated with the file, so that they
     * apply regardless of the path by which the file is accessed (assuming the principal
     * has the rights needed to traverse the path). Policies may also be applied to file paths,
     * and will be included whenever the file path matches the pattern in the path-based setting.
     *
     * The owner of the file has all rights, as does the BootPrincipal, the SystemPrincipal,
     * or any system admin principal. If the owner is a group, all descendant users of the
     * group are considered owners.
     *
     * @param rightIds a list of access right ids
     * @param principal the principal whose rights are to be determined
     * @return true if the principal has all of the rights
     */
    def hasAllRights(rightIds : ⇒ Seq[RightId], principal : Principal) : Boolean = {
        val pid = principal.getPrincipalId
        (pid == getVnode.getOwnerId) || (principal eq BootPrincipal) || (principal eq SystemPrincipal) || {
            val prights = if (pid == getPrincipalId) rights else getRightsOf(principal)
            rightIds forall (r ⇒ prights contains r)
        } || principal.isSystemAdmin_? || {
            // Last chance. The owner might be a group to which the principal belongs.
            val ancestors = principal.getAncestors
            val ownerId = getVnode.getOwnerId
            ancestors exists (_.getPrincipalId == ownerId)
        }
    }

    /**
     * Return the name of this file within its container. Some filesystems allow
     * a file to be linked under multiple containers with different names. This
     * should be the name of the file that was used in the creation of this
     * particular VFile instance whenever possible.
     *
     * @return the file name, normally unique within the container
     */
    override def getName : String = Option(getPath.getFileName) match {
        case Some(npath) ⇒ npath.toString
        case None ⇒ ""             // should only happen for root
    }

    /**
     * Get the filename path associated with this file. The Cfs filesystem supports
     * multiple paths to a file. This should be the path to the file that was used
     * in the creation of this particular CfsFile instance.
     *
     * @return the filename path
     */
    override def getPath : CfsAbsolutePath = path

    /**
     * Get the file id of this file. In Cfs this is the id of the Resource that
     * is wrapped by the CfsVnode.
     *
     * @return the file id
     */
    override def getFileId : CfsVFileId = getVnode.getFileId

    /** Get the unique resource id associated with this file */
    def getResourceId : ResourceId = getVnode.getResourceId

    /** Get file info for this file, without access control checks */
    protected def infoUnchecked(options : Option[Map[String, Any]]) : Box[CfsInfo] = Full(getVnode info options)

    /**
     * Retrieve information about the file referenced by this handle. Some
     * filesystems may support options affecting the information returned.
     *
     * @param options optional options for operation
     * @return a CfsInfo instance
     */
    override def info(options : Option[Map[String, Any]] = None) : Box[CfsInfo] = {
        (CfsFile canGetFileInfo this) (() ⇒ infoUnchecked (options))
    }

    /** Indicate when this file handle has been closed */
    protected val closed = new AtomicBoolean(false)

    protected val expectedClosed = new AtomicBoolean(false)

    /**
      * In some cases there may be nested operations which close a file more
      * than once. A call to this function after the first close suppresses
      * an error message when close() sees the file is already closed. On the
      * other hand, if this function is called, and a subsequent call to
      * close() finds the file open, that will cause an error message.
      *
      * @return
      */
    def setExpectedClosed() : Boolean = expectedClosed.getAndSet(true)

    /**
     * Close the handle. This will also close any open input or output streams,
     * and release any other resources associated with the VFile.
     */
    override def close() : Unit = {
        val vn = getVnode
        if (!closed.getAndSet(true)) {
            if (expectedClosed.get) {
                Log.error(s"file ${getPath.toString} was not closed when expected")
            }
            vn.release
        }
        else if (!expectedClosed.get) {
            Log.error(s"file handle for ${getPath.toString} is already closed")
        }
    }

    def isClosed : Boolean = closed.get()

    /** Throw an exception if an operation is attempted when the file handle is closed */
    protected def ifNotClosed[T](f : () ⇒ T) : T = {
        if (closed.get) sys.error(s"invalid operation on closed file handle for ${getPath.toString}")
        else f()
    }

    /**
     * Change the owner of this file. Only the current owner or a system administrator
     * can perform this operation. Once the owner is changed, the file is reopened
     * using the principal who opened it. This will result in a file handle that may
     * have different access rights than before, since the file is under new ownership.
     * That file handle is returned, and the original file handle is closed, so the
     * caller should discard it and use the returned handle.
     *
     * If a Failure is returned, this file handle is not closed.
     *
     * @param principal the principal who will be the new owner of the file
     * @return a boxed file handle for this file, reflecting the new owner
     */
    def chown(principal : Principal) : Box[CfsFile] = {
        val curPrincipal = getPrincipal
        val ownerId = getVnode.getOwnerId
        // Check whether the principal associated with this file handle either is the owner, or is
        // a system administrator. The owner might also be a group, in which case any descendant of
        // the group is considered an owner.
        if ((curPrincipal.getPrincipalId == ownerId) ||
            GlobalConfig.isSystemAdmin_?(curPrincipal) || {
            val ancestors = curPrincipal.getAncestors
            ancestors exists (_.getPrincipalId == ownerId)
        }) {
            // Set the new owner
            getVnode chown principal flatMap { nvnode ⇒
                nvnode.acquire
                // TODO: if open options ever are important, save them in the file handle
                // so they can be used here. But this only works if they are idempotent.
                nvnode open (getPath, curPrincipal, CfsOpenOptions.Default) match {
                    case Full(ncfsfile : CfsFile) ⇒ Full(ncfsfile)
                    case Full(vfile) ⇒
                        vfile close ()
                        Failure("chown returned a non-Cfs file")
                    case e : EmptyBox ⇒ e
                }
            } use (_ ⇒ this close ())
        }
        else AccessDeniedFailed
    }

    /**
     * Check whether this file is a container. This is always true for a CfsFile,
     * since it can be a container for metadata.
     *
     * @return true
     */
    // TODO: decide if this requires any access rights
    def isContainer_? : Boolean = getVnode.isContainer_?

    /**
     * Check whether this file is special, which is true if it has an associated
     * MimeTypeHandler.
     *
     * @return true if the file has a MimeTypeHandler
     */
    def isSpecial_? : Boolean

    /**
     * Find all the paths to the file associated with this Vnode that can be accessed
     * by a given principal.
     *
     * @return a list of CfsAbsolutePaths if successful, Nil if not
     */
    def findPaths : List[CfsAbsolutePath] = getVnode findPaths getPrincipal openOr Nil

    /**
     * If a given file is a member of this container, get the name it has in this
     * container, which may be different than the name in its file handle path.
     *
     * @param member the potential member of this container
     * @return the boxed name of the member if it is a member of this container,
     *         Empty if it is not a member, or Failure on error
     */
    def getMemberName(member : CfsFile) : Box[String] = {
        // TODO: determine if this needs access control
        getVnode getMemberName member.getVnode
    }

    /**
     * Check whether a given file is a member of this container.
     *
     * @param member the potential member of this container
     * @return true if it is a member
     */
    def contains_?(member : CfsFile) : Boolean = getMemberName(member).isDefined

    /**
     * Attempt to mount the specified Vfs filesystem at the file referenced by this handle.
     * Not all filesystems will support this operation, and those that do are likely to
     * restrict mount points to container files. Some options for the mount may be supported,
     * such as mounting read-only, or specifying a password for an encrypted filesystem.
     *
     * @param vfs the filesystem to be mounted
     * @param options optional options that may affect the mount
     */
    def mount(vfs : Vfs, options : Option[Map[String, Any]] = None) : Box[VFile] = {
        Failure("mount is not yet implemented")
    }

    def isType_?(mimeType : String) : Boolean = (CfsFile canGetFileInfo this) { () ⇒
        Full(getVnode.getMimeType == mimeType)
    } openOr false

    def getMimeType : Box[String] = (CfsFile canGetFileInfo this) (() ⇒ Full(getVnode.getMimeType))

    def getPolicies(principal : Principal) : List[ResourceId] = getVnode.getPolicies

    def getPathPolicies : List[PathPolicy] = pathPolicies

    def withParentFile[T](options : CfsOpenOptions = CfsOpenOptions.Default)(f : CfsDirFile ⇒ Box[T]) : Box[T] = {
        val ppath = getPath.getParent
        Cfs open (ppath, getPrincipal, options) match {
            case Full(dirfile : CfsDirFile) ⇒
                val result = f(dirfile)
                dirfile close ()
                result
            case Full(cfsfile : CfsFile) ⇒
                cfsfile close ()
                Failure(s"${ppath.toString} is not a container")
            case Full(vfile : VFile) ⇒
                vfile close ()
                Failure(s"${path.toString} is not a Cfs file")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Set the value of a specified attribute for this file. The attribute is identified
     * by its id. The expected type of the attribute also can be specified, in which case
     * it must match the actual type of the attribute. An attempt is made to decode the
     * given attribute value to a value that is consistent with the attribute type, if it
     * is not already consistent. So for example, a numeric attribute value can be passed
     * as a string, provided the string can be parsed to a number. If the specified value
     * is null (or None, Empty, or Nil), any existing value of the attribute for this file
     * is removed.
     *
     * @param id the unique attribute id
     * @param atype the expected attribute type, or None
     * @param value the new value for the attribute
     * @return a boxed AttributeInfo with a null value
     */
    override def setAttribute(id : AttributeId, atype : Option[AttributeType], value : Any) : Box[AttributeInfo] = {
        (CfsFile canGetFileInfo this) { () ⇒
            if (canWriteAttribute(id)) {
                AttrDef getAttributeType id flatMap { actualType ⇒
                    if ((atype fold true)(_ == actualType)) {
                        AttrVal setValue (id, actualType, getResourceId, value, getPrincipalId) match {
                            case Full(aval) ⇒
                                aval.getValue map { avvalue ⇒
                                    AttributeInfo(id, actualType, avvalue, aval.setter.get, aval.stime.get)
                                }
                            case Empty ⇒
                                if (AttrVal isEmptyValue_? value) {
                                    Full(AttributeInfo(id, actualType, null, getPrincipalId, millis))
                                }
                                else Failure(s"result is unexpectedly Empty")
                            case f : Failure ⇒ f
                        }
                    }
                    else Failure(s"attribute id ${id.id} is not type ${atype.get}")
                }
            }
            else Failure("access denied")
        }
    }

    /**
     * Return the value of a particular attribute, identified by its id. The expected
     * type of the attribute also can be specified, in which case it must match the
     * actual type of the attribute.
     *
     * @param id the unique attribute id
     * @param atype the expected attribute type, or None
     * @return a boxed AttributeInfo containing the value associated with this file. If
     *         there is no value for the attribute associated with this file, an
     *         AttributeInfo containing null for the value is returned.
     *         A Failure is returned if the attribute id is undefined, or the current
     *         file principal lacks the rights to access the value.
     */
    override def getAttribute(id : AttributeId,
                              atype : Option[AttributeType]) : Box[AttributeInfo] = (CfsFile canGetFileInfo this) { () ⇒
        if (canReadAttribute(id)) {
            AttrVal get (id, getResourceId) match {
                case Full(avalue) ⇒
                    avalue.getValue map { avvalue ⇒
                        AttributeInfo(id, avalue.atype.get, avvalue, avalue.setter.get, avalue.stime.get)
                    }
                case Empty ⇒
                    AttrDef getAttributeType id map { actualType ⇒
                        AttributeInfo(id, actualType, null, 0L, 0L)
                    }
                case f : Failure ⇒ f
            }
        }
        else Failure("access denied")
    }

    /**
     * Get a list of metadata values associated with this file. Each value is returned
     * as a triple containing the unique id associated with the corresponding attribute
     * definition, the attribute type, and the value as a type that is consistent with
     * the attribute type. Only attributes for which the current file principal has
     * read rights are returned.
     *
     * @return a boxed list of AttributeInfo, or Failure. An empty list is returned if there
     *         are no metadata attributes, not Empty.
     */
    override def listAttributes : Box[List[AttributeInfo]] = (CfsFile canGetFileInfo this) { () ⇒
        def helper(list : List[AttrVal], acc : List[AttributeInfo]) : List[AttributeInfo] = {
            list match {
                case Nil ⇒ acc
                case head :: tail ⇒
                    val id = head.id.get
                    if (canReadAttribute(id)) {
                        head.getValue match {
                            case Full(avalue) ⇒
                                helper(tail,
                                       AttributeInfo(id, head.atype.get, avalue,
                                                     head.setter.get, head.stime.get) :: acc)
                            case _ : EmptyBox ⇒ helper(tail, acc)
                        }
                    }
                    else helper(tail, acc)
            }
        }
        Full(helper(AttrVal getAll getResourceId, Nil))
    }

    /**
     * Check whether the file has any associated metadata attributes. This may be
     * subject to access control, which could cause Failure to be returned.
     *
     * @return a boxed value of true if the file has attributes, or Failure if the
     *         determination cannot be made
     */
    override def hasAttributes : Box[Boolean] = (CfsFile canGetFileInfo this) { () ⇒
        Full(AttrVal hasValues getResourceId)
    }

    protected def canReadAttribute(id : AttributeId) : Boolean = {
        val p = getPrincipal
        (p.getPrincipalId == getVnode.getOwnerId) || p.isSystemAdmin_? ||
            ((CfsFile canReadAttributes this) { () ⇒ Full(true) } openOr false)
    }

    protected def canWriteAttribute(id : AttributeId) : Boolean = {
        val p = getPrincipal
        (p.getPrincipalId == getVnode.getOwnerId) || p.isSystemAdmin_? ||
            ((CfsFile canWriteAttributes this) { () ⇒ Full(true) } openOr false)
    }

    def asMap : Map[String, Any] = (CfsFile canGetFileInfo this) { () ⇒

        val map = getVnode.getResource.asMap ++ Map("name" → getName,
                                                    "special" → isSpecial_?,
                                                    "container" → isContainer_?,
                                                    "path" → path.toString,
                                                    "paths" → (findPaths map (_.toString)))
        Full(map)
    } openOr Map()

    protected def getMimeTypeHandler : Box[MimeTypeHandler] = {
        DbManager getMimeTypeHandler getVnode.getMimeType
    }
}

object CfsFile extends Module {

    val Log : Logger = Cfs.Log

    def getName : String = "Common Choice File"

    /**
     * Get a list of schemas used by this module, to be included in the Boot
     * schemify step.
     *
     * @return a list of Mapper objects representing database tables
     */
    def getSchemas : List[BaseMetaMapper] = Nil

    /**
     * Return a list of rights definitions for this module.
     *
     * @return list of rights definitions
     */
    def getRights : List[RightDef] = List(
        RightDef("get_file_info", "Choice file", "get basic file metadata"),
        RightDef("set_file_info", "Choice file", "change basic file metadata"),
        RightDef("read_file_attributes", "Choice file", "read file attributes"),
        RightDef("write_file_attributes", "Choice file", "write file attributes")
    )

    /**
     * This allows read access to basic file metadata, including determining whether
     * there are any attribute values for the file (but not to read those values).
     */
    val canGetFileInfo = AnyRightsCheck("get_file_info")

    /**
     * This allows write access to certain file metadata, such as the MIME type.
     */
    val canSetFileInfo = AnyRightsCheck("set_file_info")

    /**
     * This allows read access to all attribute values associated with a file.
     */
    val canReadAttributes = AnyRightsCheck("read_file_attributes")

    /**
     * This allows write access to all attribute values associated with a file.
     */
    val canWriteAttributes = AnyRightsCheck("write_file_attributes")
}
