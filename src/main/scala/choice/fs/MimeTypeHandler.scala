/**
  * Copyright © 2012-2018 The Board of Trustees of The Leland Stanford Junior University.
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

import _root_.choice.model._
import choice.access.{Module, RightDef, RightsCheck}
import choice.actor.{DbManager, DbManagerPlugin}
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.mapper.BaseMetaMapper

/**
 * A MimeTypeHandler provides services for file resources of a particular MIME type.
 * All MimeTypeHandlers are registered with the DB filesystem via the DbManager.
 */
trait MimeTypeHandler extends Module {

    implicit val formats : DefaultFormats.type = DefaultFormats
    
    protected val dbPlugin : DbManagerPlugin = DbManager registerMimeTypeHandler this

    /**
     * MIME type strings are associated with a unique id via a DB table.
     */
    private lazy val mtid = MimeType findOrCreate getMimeType

    override def init() : Unit = {
        super.init()
        Log.info(s"defined MIME type '$getMimeType with id $mtid")
    }

    /**
     * Get the string representation of the MIME type associated with this handler.
     */
    def getMimeType : String
    
    /**
     * Get the id for the MIME type.
     */
    def getMimeTypeId : MimeTypeId = mtid.id
    
    /**
     * Check whether a Resource has the MIME type associated with this handler.
     */
    def isType_?(mtid : MimeTypeId) : Boolean = mtid == getMimeTypeId
    
    def isType_?(resource : Resource) : Boolean = resource.getMimeTypeId == getMimeTypeId

    /**
     * Indicate whether this MIME type represents some type of container for other objects.
     * For example, Folder is a general container for files. GroupInfo represents a user
     * group, and can contain other user groups and users.
     */
    def isContainer_? : Boolean = false

    /**
     * Most files that have a MIME type handler are special. Mainly that means that their
     * MIME type should be immutable after they are created. CfsPlain files are generally
     * not special.
     *
     * @return true by default
     */
    def isSpecial_? : Boolean = true

    /**
     * Wrap a Resource object in an instance of the HandlerType for this MIME type handler.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    def instantiate(resource : Resource) : Box[CfsVnode]

    /**
     * This method is called by DbManager when it is about to delete a file with a MIME
     * type associated with this MimeTypeHandler. It gives the MimeTypeHandler an
     * opportunity to veto the delete, or to do other cleanup operations, such as
     * removing non-filesystem references to the file.
     *
     * Note that this is called on the DbManager thread, so it should only use the
     * dbPlugin interface to access DbManager functions.
     *
     * @param vnode the vnode of the file about to be deleted
     * @return a boxed value of true if the delete should proceed, otherwise a
     *         boxed false or EmptyBox
     */
    def delete(vnode : CfsVnode) : Box[Boolean] = {
        // Remove any access control policy associations with this file
        vnode.removeAllPolicies()
        Full(true)
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    def canCreateObject : RightsCheck

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container from which the object is
     * to be unlinked.
     *
     * @return a RightsCheck instance
     */
    def canUnlinkObject : RightsCheck
}

trait ContainerMimeType extends MimeTypeHandler {
    /**
     *  This defines the rights needed to list members of the container, or to reference
     *  them by name.
     *
     * @return a RightsCheck instance
     */
    def canListMember : RightsCheck

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    def canAddMember : RightsCheck

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    def canRemoveMember : RightsCheck
}

object MimeTypeHandler extends Module {
    override val Log = Logger("choice.fs.MimeTypeHandler")
    override val getName = "MimeTypeHandler"

    override val getSchemas : List[BaseMetaMapper] = List(MimeType)

    override val getRights : List[RightDef]  = Nil
}
