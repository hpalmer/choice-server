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
import choice.fs.vfs.CanHazMap
import choice.model.{GlobalConfig, ResourceId}
import net.liftweb.common.{Full, _}

import scala.annotation.tailrec

/**
 * A principal represents an entity or collection of entities which can initiate
 * operations which are subject to access control. Typical principals are users
 * and user groups.
 */
trait Principal extends CanHazMap {
    /**
     * Get the ResourceId of this principal. Note that this is not necessarily the
     * ResourceId associated with the Principal returned by getPrincipal. That
     * is the principal on whose behalf the current Principal resource is being
     * accessed.
     */
    def getPrincipalId : ResourceId

    /**
     * A principal can be a member of a container, which is itself a principal.
     * For example, a user can be a member of user groups, which are also
     * principals. This returns Principal handles for all the container
     * principals which include this principal as a member, and which are
     * accessible by the principal of this handle.
     */
    def getParents : List[Principal]

    /**
     * Get the name of this principal.
     */
    def getPrincipalName : String

    /**
     * This principal represents a group of principals.
     *
     * @return false for a singular principal, otherwise true
     */
    def isGroup_? : Boolean

    /**
     * Create a stream containing the ancestors of this principal (starting with
     * the principal itself), in breadth-first order.
     */
    def getAncestors : Stream[Principal]

    /**
     * Check whether this principal is a descendant of a given principal, where
     * a principal is considered a descendant of itself. This is determined by
     * a breadth-first search of the ancestors of this principal.
     *
     * @param principal the resource id of a potential ancestor principal
     * @return true if the given principal is an ancestor of this principal
     */
    def isDescendantOf(principal : ResourceId) : Boolean = {
        getAncestors exists (principal == _.getPrincipalId)
    }

    /**
     * Check whether this principal is a system administrator. This includes
     * a special bootstrap principal, as well as members of the system administrators
     * group.
     *
     * @return true if this principal has system administrator status
     */
    def isSystemAdmin_? : Boolean = GlobalConfig.isSystemAdmin_?(this)

    /**
     * Return a description of this principal as a map.
     *
     * @return map containing principal properties
     */
    def asMap : Map[String, Any] = {
        Map("id" → getPrincipalId.id, "name" → getPrincipalName, "isgroup" → isGroup_?,
            "isadmin" → isSystemAdmin_?)
    }
}

/**
 * This is a principal represented by a Cfs file. The principal
 * interface extends the CfsVnode of the file.
 */
class CfsPrincipal(resourceId : ResourceId, path : Option[CfsAbsolutePath], isGroup : Boolean) extends Principal {

    lazy val vpath : CfsAbsolutePath = path match {
        case Some(p) => p
        case None =>
            (Cfs withVnode resourceId) { vnode =>
                vnode findPath this
            } openOr CfsAbsolutePath(CfsRootRoot, List("*unknown*"))
    }

    def getPrincipalPath : CfsAbsolutePath = vpath

    override def getPrincipalId : ResourceId = resourceId

    override def isGroup_? : Boolean = isGroup

    /**
     * Get the containers of this principal that are user groups. The returned
     * containers are acquired.
     *
     * @return
     */
    override def getParents : List[Principal] = {
        (Cfs withVnode resourceId) { vnode =>
            vnode.findParents match {
                case Full(list) =>
                    val plist = (list foldLeft List[Principal]()) { (list, pair) =>
                        pair match {
                            case (pnode : IsPrincipal, _) =>
                                val result = pnode.getSelfPrincipal
                                pnode.release
                                result :: list
                            case (notpnode, _) =>
                                notpnode.release
                                list
                        }
                    }
                    Full(plist)
                case e : EmptyBox => Failure(s"failed to get parents of $getPrincipalName", None, e)
            }
        } openOr Nil
    }

    override def getPrincipalName : String = vpath.toString

    /**
     * Make a stream of all the ancestors of this principal.
     * This must be a lazy val because of the reference to GuestPrincipal, which uses this class.
     */
    override lazy val getAncestors : Stream[Principal] = {
        @tailrec
        def getUniqueParents(acc : List[Principal], remain : List[Principal]) : List[Principal] = {
            remain match {
                case Nil => acc
                case head :: tail =>
                    val parents = head.getParents.filterNot { p =>
                        acc exists (_.getPrincipalId == p.getPrincipalId)
                    }
                    getUniqueParents(acc ::: parents, tail)
            }
        }
        def helper(update : Boolean, remain : List[Principal]) : Stream[Principal] = {
            remain match {
                case Nil => Stream.Empty
                case _ =>
                    if (update) helper(update = false, getUniqueParents(Nil, remain))
                    else remain.toStream #::: helper(update = true, remain)
            }
        }
        // Any users except guest are implicitly in the anyone group for access control
        // purposes.
        val initlist =
            if (isGroup_? || (this eq GuestPrincipal)) List(this)
            else List(this, AnyonePrincipal)
        val result = helper(update = false, initlist)
        result
    }

    override def asMap : Map[String, Any] = {
        super.asMap + ("path" → getPrincipalPath.toString)
    }
}

object CfsPrincipal {
    val Log = Logger("choice.access.CfsPrincipal")

    def apply(resource : ResourceId) : CfsPrincipal = {
        // TODO: 2017/02/11 defaulting to GuestPrincipal is not correct in all cases. The
        // simplest fix is probably to have the caller specify the default to be used
        // when the given resource no longer exists. A separate question is whether Resource
        // entries should be allowed to reference owners who no longer exist.
        (Cfs withVnode resource) { vnode => Full(CfsPrincipal(vnode)) } openOr GuestPrincipal
    }

    def apply(vnode : CfsVnode) : CfsPrincipal = {
        new CfsPrincipal(vnode.getResourceId, None, isGroup_?(vnode))
    }

    def apply(file : CfsFile) : CfsPrincipal = {
        new CfsPrincipal(file.getFileId.resource, Some(file.getPath), isGroup_?(file.getVnode))
    }

    def isGroup_?(vnode : CfsVnode) : Boolean = vnode match {
        case _ : UserNode => false
        case _ : GroupNode => true
        case _ => false
    }
}

/**
 * This is a trait of an object which itself represents a principal. This is to be
 * distinguished from the case of an object which records the principal of its
 * creator.
 */
trait IsPrincipal {
    def getSelfPrincipal : Principal
    def getSelfPrincipalId : ResourceId = getSelfPrincipal.getPrincipalId
}

/**
 * This is a trait of an object which records the principal responsible for
 * creating it. This same principal is typically taken to be responsible
 * for operations on the object.
 */
trait HasPrincipal {
    def getPrincipal : Principal
    def getPrincipalId : ResourceId = getPrincipal.getPrincipalId
}
