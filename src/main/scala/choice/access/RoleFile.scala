/**
  * Copyright © 2013-2017 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.access

import choice.fs._
import choice.fs.vfs.Vnode
import choice.lib.ExtendedBox._
import choice.model.{Resource, ResourceId}
import net.liftweb.common.{Full, _}

/**
 * This defines a special type of container file which represents a role. A role
 * is associated with a set of rights, represented by RoleFile members of the
 * container. A policy associates roles with principals.
 */

class RoleId(val id : Long) extends AnyVal

object RoleId {
    import scala.language.implicitConversions
    implicit def roleIdToLong(roleId : RoleId) : Long = roleId.id
    implicit def roleIdToResourceId(roleId : RoleId) : ResourceId = ResourceId(roleId.id)

    def apply(resid : Long) : RoleId = new RoleId(resid)
}

case class RoleDesc(description : String)

class RoleNode(resource : Resource) extends CfsSpecialNode(resource)
    with AtomicDataNode[RoleNode] with JsonDataNode[RoleNode, RoleDesc] {

    val mf : Manifest[RoleDesc] = manifest[RoleDesc]

    def getRoleId : RoleId = RoleId(getResourceId.id)

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[RoleFile] = {
        Full(new RoleFile (path, principal, this))
    }

//    /**
//     * Create a new member in this container with a given name. A specified
//     * MIME type may affect the specific type of Vnode returned. The MIME
//     * type also may prevent the file from being created, if the filesystem
//     * or the container doesn't support the specified type.
//     *
//     * The reference count of the returned Vnode will have been incremented,
//     * but it will not be 'open' for read or write operations. At some point
//     * the caller should call its release() method.
//     *
//     * @param name the name for the new member, which must not already exist,
//     *             unless the filesystem implements versioning
//     * @param principal the principal responsible for the operation
//     * @param mimeType the MIME type associated with the new file
//     * @param options options that may affect the create
//     * @return a boxed Vnode if the file is created successfully, or a
//     *         Failure otherwise. Empty should not be returned.
//     */
//    override def create(name : String, principal : Principal,
//                        mimeType : String, options : CfsCreateOptions) : Box[RightNode] = {
//        super.create(name, principal, mimeType, options) match {
//            case Full(rnode : RightNode) ⇒
//                addRightDef (rnode.getRightDef)
//                Full(rnode)
//            case Full(cfsnode) ⇒
//                cfsnode.release
//                Failure(s"a role can only contain rights")
//            case e : EmptyBox ⇒ e
//        }
//    }

    /**
     * Link a given Vnode as a member of this container with a specified name.
     * Currently the member Vnode must be a CfsVnode.
     *
     * @param name the filename given to the Vnode in this container
     * @param member the Vnode to be linked as a member
     * @return the boxed name of member if successful, or Failure on error.
     */
    override def link(name: String, member: Vnode): Box[String] = {
        super.link(name, member) use { _ ⇒
            member match {
                case rnode : RightNode ⇒ addRightDef (rnode.getRightDef)
                case _ ⇒
            }
        }
    }

    /**
     * Unlink a member of this container. The member may continue to exist if there are
     * other links to it. And even if there are no other links, the member will not be
     * deleted until the last open file handle for it is closed.
     *
     * The caller should have file handles for this container and the member open, and
     * both associated Vnodes should be write-locked.
     *
     * @param name the member name
     * @param member the member Vnode
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container.
     */
    override def unlink(name : String, member : Vnode) : Box[(Boolean, List[String])] = {
        super.unlink(name, member) use {
            case (unlinked, _) ⇒
                if (unlinked) member match {
                    case rnode : RightNode ⇒ removeRightDef (rnode.getRightDef)
                    case _ ⇒
                }
        }
    }

    private var _rightDefs : Option[List[RightDef]] = None

    private def getRightDefsUnlocked : List[RightDef] = _rightDefs match {
        case Some(list) ⇒ list
        case None ⇒
            val list = RoleFile getRoleRights getRoleId
            _rightDefs = Some(list)
            list
    }

    def getRightDefs : List[RightDef] = {
        withReadLock { () ⇒ _rightDefs } match {
            case Some(list) ⇒ list
            case None ⇒ withWriteLock { () ⇒ getRightDefsUnlocked }
        }
    }

    def addRightDef(rdef : RightDef) : List[RightDef] = withWriteLock { () ⇒
        val list = getRightDefsUnlocked
        if (list exists (_.name == rdef.name)) list
        else {
            val newlist = rdef :: list
            _rightDefs = Some(newlist)
            newlist
        }
    }

    def removeRightDef(rdef : RightDef) : List[RightDef] = withWriteLock { () ⇒
        val list = getRightDefsUnlocked
        val (remove, remaining) = list partition (_.name == rdef.name)
        remove match {
            case Nil ⇒ remaining
            case _ ⇒
                _rightDefs = Some(remaining)
                remaining
        }
    }
}

class RoleFile(path : CfsAbsolutePath, principal : Principal, vnode : RoleNode)
    extends CfsSpecial(path, principal, vnode) with CfsDirFile
    with JsonDataFile[RoleFile, RoleNode, RoleDesc] {

    override def getVnode : RoleNode = vnode

    def getRoleId : RoleId = getVnode.getRoleId

    def addRight(name : String, principal : Principal) : Box[RightDef] = (RoleFile canAddMember this) { () ⇒
        RightFile.getRightFile(name, principal) flatMap { right ⇒
            val result = link (name, right) flatMap { vfile ⇒
                val rdef = right.getRightDef
                vfile close ()
                getVnode addRightDef rdef
                Full(rdef)
            }
            right close ()
            result
        }
    }

    def removeRight(name : String) : Box[Boolean] = unlink (name, recursive = false)

    /**
     * Unlink a member from this container. If this is the last link to the member,
     * it is deleted from the filesystem, and a recursive unlink is performed on its
     * metadata (if metadata is supported). If the member is itself a container, it
     * must be empty (except for metadata), or else the recursive option must be
     * true. A recursive unlink may be partially completed before returning an error.
     *
     * Override the standard unlink() in order to avoid checking RightFiles's
     * canUnlinkObject. We should never be removing the last link to a right here.
     *
     * @param member the name of the member to be unlinked from this container
     * @param recursive if the member is a container, recursively unlink its members
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container, or the principal lacked
     *         the proper access rights, or on any other error.
     */
    override def unlink(member : String,
                        recursive : Boolean) : Box[Boolean] = (RoleFile canRemoveMember this) { () ⇒
        withMember(member) {
            case rightfile : RightFile ⇒
                // Rights are the only thing roles can contain.
                // Rights should also be members of the /System/Rights folder, so this
                // should never be removing the last link to a right.
                assert(rightfile.getVnode.getResource.getRefCount >= 2)
                super.unlinkUnchecked(rightfile, recursive)
        }
    }

    def getData : Box[RoleDesc] = (RoleFile canReadDescription this) (() ⇒ getDataUnchecked)

    def putData(data : RoleDesc) : Box[Boolean] = (RoleFile canWriteDescription this) (() ⇒ putDataUnchecked (data))

    def update(desc : RoleDesc) : Box[RoleFile] = {
        val name = getName
        (this putData desc) map (_ ⇒ this) or Failure(s"RoleFile $name update failed")
    }
}

object RoleFile extends ContainerMimeType {
    import choice.core.Startup.RolesFolderPath

    override protected val Log = Logger("choice.access.RoleFile")

    override val getName = "Role File Type"

    override val getSchemas = Nil

    override val getMimeType = "choice/role"

    override val getRights = List(
        RightDef("create_role", "role file", "create a role"),
        RightDef("unlink_role", "role file", "unlink a role"),
        RightDef("read_role_desc", "role file", "read role description"),
        RightDef("write_role_desc", "role file", "write role description"),
        RightDef("list_role_rights", "role file", "list role rights"),
        RightDef("add_role_right", "role file", "add role rights"),
        RightDef("remove_role_right", "role file", "remove role rights")
    )

    def apply(name : String, description : String, principal : Principal) : Box[RoleFile] = {
        val rfolder = getRolesFolder (principal)
        val mydesc = RoleDesc(description)
        val rfile = rfolder getMember name match {
            case Full(role : RoleFile) ⇒
                role.getData match {
                    case Full(rdesc) ⇒
                        if (rdesc != mydesc) {
                            Log.warn(s"RoleFile $name does not match existing file - attempting to update")
                            role update mydesc
                        }
                        else Full(role)
                    case Empty ⇒
                        Log.warn(s"RoleFile $name is missing descriptor - attempting to update")
                        role update mydesc
                    case f : Failure ⇒
                        Log.error(s"RoleFile $name descriptor read error - ${f.toString}")
                        role update mydesc
                }
            case Full(other) ⇒
                other close ()
                Failure(s"RoleFile $name conflicts with existing non-role file")
            case Empty ⇒
                rfolder create (name, getMimeType, CfsCreateOptions.Default) match {
                    case Full(role : RoleFile) ⇒ role update mydesc
                    case Full(other) ⇒
                        other close ()
                        Failure(s"RightFile $name creation return non-role file")
                    case e : EmptyBox ⇒ Failure(s"RoleFile $name creation failed - ${e.toString}")
                }
            case f : Failure ⇒
                Log.error(s"RoleFile $name error - ${f.toString}")
                f
        }
        rfolder close ()
        rfile
    }

    def createRole(name : String, description : String, principal : Principal) : Box[RoleFile] = {
        RoleFile(name, description, principal)
    }

    override def instantiate(resource : Resource) : Box[RoleNode] = {
        if (isType_?(resource)) Full(new RoleNode(resource))
        else Failure(s"resource id ${resource.getSafeKey.id} is not a role")
    }

    def getRolesFolder(principal : Principal) : CfsFolder = {
        Cfs.withValidPath(RolesFolderPath) { cfspath ⇒
            Cfs open (cfspath, principal, CfsOpenOptions.Default) match {
                case Full(rfolder : CfsFolder) ⇒ Full(rfolder)
                case Full(vfile) ⇒
                    vfile close ()
                    sys.error(s"$RolesFolderPath is not a folder")
                case Empty ⇒ sys.error(s"$RolesFolderPath does not exist")
                case f : Failure ⇒ sys.error(s"error opening $RolesFolderPath: ${f.msg}")
            }
        } openOr sys.error(s"$RolesFolderPath is not a valid Cfs path")
    }

    def getRoleId(name : String) : Box[RoleId] = {
        Cfs.withValidPath(RolesFolderPath) { cfspath ⇒
            Cfs.withExistingFile(cfspath / name, SystemPrincipal) {
                case rfile : RoleFile ⇒ Full(RoleId(rfile.getFileId.resource))
            }
        }
    }

    def getRoleFile(role : RoleId) : Box[RoleFile] = {
        Cfs open (CfsVFileId(role), SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(rfile : RoleFile) ⇒ Full(rfile)
            case Full(vfile) ⇒
                vfile close ()
                Failure("invalid role id")
            case Empty ⇒ Failure("no such role")
            case f : Failure ⇒ f
        }
    }

    /**
     * Get all the rights associated with a role that is identified by its id.
     *
     * @param role the role id
     * @return a list of rights associated with the role
     */
    def getRoleRights(role : RoleId) : List[RightDef] = {
        Cfs open (CfsVFileId(role), SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(rfile : RoleFile) ⇒
                val rdefs = rfile.getMembers(Some(RightFile.getMimeType)) match {
                    case Full(names) ⇒
                        names flatMap { rname ⇒
                            rfile.getMember(rname) match {
                                case Full(right : RightFile) ⇒
                                    val result = List(right.getRightDef)
                                    right close ()
                                    result
                                case Full(vfile) ⇒
                                    vfile close ()
                                    Nil
                                case _ : EmptyBox ⇒ Nil
                            }
                        }
                    case _ : EmptyBox ⇒ Nil
                }
                rfile close ()
                rdefs.toList
            case Full(vfile) ⇒
                vfile close ()
                Nil
            case _ : EmptyBox ⇒ Nil
        }
    }

    /**
     * This defines the rights needed to list members of the container, or to reference
     * them by name.
     *
     * @return a RightsCheck instance
     */
    val canListMember : RightsCheck = AnyRightsCheck("list_role_rights", "add_role_right", "remove_role_right")

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    val canAddMember : RightsCheck = AnyRightsCheck("add_role_right")

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    val canRemoveMember : RightsCheck = AnyRightsCheck("remove_role_right")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_role")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_role")

    val canReadDescription = AnyRightsCheck("read_role_desc", "write_role_desc")
    val canWriteDescription = AnyRightsCheck("write_role_desc")
}