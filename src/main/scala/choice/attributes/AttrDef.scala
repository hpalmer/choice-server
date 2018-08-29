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
package choice.attributes

import choice.access.{RightDef, _}
import choice.fs.{CfsAbsolutePath, _}
import choice.fs.vfs.AttributeType.AttributeType
import choice.fs.vfs.{AttributeId, AttributeType}
import choice.model.Resource
import net.liftweb.common.{Full, _}
import net.liftweb.json._
import net.liftweb.mapper.{BaseMetaMapper, By}
import net.liftweb.util.Helpers.millis

import scala.reflect.ClassTag

case class AttrDefInfo(atype : AttributeType, description : String)

class AttrDefCreateOptions(val atype : AttributeType, val description : Option[String],
                           override val dataSeqnum : Long, override val ctime : Long,
                           override val altid : Long, override val replace : Boolean)
    extends CfsCreateOptions(dataSeqnum, ctime, altid, replace)

object AttrDefCreateOptions {
    def apply(atype : AttributeType, description : Option[String] = None,
              dataSeqnum : Long = 0L, ctime : Long = millis,
              altid : Long = 0L, replace : Boolean = false) : AttrDefCreateOptions = {
        new AttrDefCreateOptions(atype = atype, description = description, dataSeqnum = dataSeqnum,
                                 ctime = ctime, altid = altid, replace = replace)
    }
}

class AttrDefNode(resource: Resource) extends CfsSpecialNode(resource)
                                              with AtomicDataNode[AttrDefNode]
                                              with JsonDataNode[AttrDefNode, AttrDefInfo] {
    val mf : Manifest[AttrDefInfo] = manifest[AttrDefInfo]

    class EnumerationSerializer[E <: Enumeration: ClassTag](enum: E) extends Serializer[E#Value] {
        val EnumerationClass : Class[E#Value] = classOf[E#Value]
        val formats : AnyRef with Formats = Serialization.formats(NoTypeHints)
        def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), E#Value] = {
            case (TypeInfo(EnumerationClass, _), json) ⇒ json match {
                case JString(value) ⇒ enum.withName(value)
                case value          ⇒ throw new MappingException("Can't convert " + value + " to " + EnumerationClass)
            }
        }
        def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
            case i: E#Value ⇒ JString(i.toString)
        }
    }

    override implicit val formats : Formats = DefaultFormats + new EnumerationSerializer(AttributeType)

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * a CfsFile that references this CfsVnode. Using the CfsFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * @param path the path that led to this CfsVnode
     * @param principal the principal to be associated with the CfsFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting CfsFile
     * @return a boxed CfsFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    override def cfsOpen(path: CfsAbsolutePath, principal: Principal,
                         options: CfsOpenOptions): Box[AttrDef] = {
        assert(this.isReferenced_?)
        Full(new AttrDef (path, principal, this))
    }

    /**
     * Subclasses override this function in order to do custom file creation processing.
     * That may include processing create options which are specific to the file type.
     * The subclass also has the option to return a different Vnode than the current one.
     * If it does, the current Vnode should be released, and the returned Vnode should be
     * acquired.
     *
     * @param name the requested name of the member (which may be changed later via an
     *             overload of the acceptName_? method)
     * @param principal the principal requesting the file creation
     * @param options options for the file creation, possibly file type specific
     * @return a boxed CfsVnode (usually this one) if successful, otherwise Failure
     */
    override def cfsCreate(name: String, principal: Principal, options: CfsCreateOptions): Box[CfsVnode] = {
        assert(this.isReferenced_?)
        options match {
            case adefOptions : AttrDefCreateOptions ⇒
                putData(AttrDefInfo(adefOptions.atype, adefOptions.description getOrElse ""))
            case _ ⇒
                Failure(s"missing attribute type needed to create attribute '$name'")
        }
    }
}

class AttrDef(path: CfsAbsolutePath, principal: Principal, vnode: AttrDefNode)
    extends CfsSpecial(path, principal, vnode)
            with JsonDataFile[AttrDef, AttrDefNode, AttrDefInfo] {

    override def putData(data: AttrDefInfo): Box[Boolean] = (AttrDef canModifyAttribute this) { () ⇒
        getDataUnchecked flatMap { ainfo ⇒
            if (ainfo.atype == data.atype) {
                putDataUnchecked (AttrDefInfo(ainfo.atype, data.description))
            }
            else Failure(s"cannot change attribute $getPath type from ${ainfo.atype} to ${data.atype}")
        }
    }

    override def getData: Box[AttrDefInfo] = (AttrDef canInspectAttribute this) { () ⇒
        getDataUnchecked
    }

    def getValues : List[AttrVal] = {
        AttrVal findAll By(AttrVal.adef, getResourceId.id)
    }

    override def asMap : Map[String, Any] = {
        val amap = getDataUnchecked map { ainfo ⇒
            Map("type" → ainfo.atype.toString, "description" → ainfo.description)
        } openOr Map()
        super.asMap ++ amap
    }
}

object AttrDef extends MimeTypeHandler {
    override protected val Log = Logger("choice.attributes.AttrDef")

    /**
     * Get a list of schemas used by this module, to be included in the Boot
     * schemify step.
     *
     * @return a list of Mapper objects representing database tables
     */
    override val getSchemas: List[BaseMetaMapper] = List(AttrVal)

    override val getName: String = "Attribute Definition File Type"

    /**
     * Get the string representation of the MIME type associated with this handler.
     */
    override val getMimeType: String = "choice/attribute"

    /**
     * Return a list of rights definitions for this module.
     *
     * @return list of rights definitions
     */
    override val getRights: List[RightDef] = List(
        RightDef("define_attribute", "attribute definition", "define an attribute"),
        RightDef("inspect_attribute_def", "attribute definition", "read an attribute definition"),
        RightDef("remove_attribute_def", "attribute definition", "remove an attribute definition and values"),
        RightDef("modify_attribute_def", "attribute definition", "modify an attribute definition")
    )

    /**
     * Wrap a Resource object in an instance of the HandlerType for this MIME type handler.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    override def instantiate(resource: Resource): Box[AttrDefNode] = {
        if (isType_?(resource)) Full(new AttrDefNode(resource))
        else Failure("AttrDef instantiate passed a non-AttrDef resource")
    }

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
    override def delete(vnode: CfsVnode): Box[Boolean] = {
        // Delete all the attribute values associated with this attribute definition
        AttrVal bulkDelete_!! By(AttrVal.adef, vnode.getResource)
        super.delete(vnode)
    }

    /**
     * Determine if there are any value assignments for a given attribute definition,
     * specified by its attribute id.
     *
     * @param id the attribute id
     * @return true if any value assignments exist for the attribute, else false
     */
    def hasValues(id : AttributeId) : Boolean = {
        AttrVal find By(AttrVal.adef, id.id) map (_ ⇒ true) openOr false
    }

    /**
     * Convert an attribute path to an absolute path of an attribute definition file.
     * The given path may be relative or absolute. If it is relative, it is taken as
     * relative to "/System/Attributes".
     *
     * @param path the absolute or relative path string
     * @return a boxed absolute path, or Failure if the path syntax is bad
     */
    def getAttributePath(path : String) : Box[CfsAbsolutePath] = Cfs.withValidPath (path) { cfspath ⇒
        Full(CfsAbsolutePath(CfsRootRoot, List("System", "Attributes")) resolve cfspath)
    }

    /**
     * Open an attribute definition file, given its path. If the path is not an absolute
     * path, it is taken to be relative to "/System/Attributes".
     *
     * @param path the path to the attribute definition file
     * @param principal the principal responsible for this operation
     * @param options open options
     * @return a boxed file handle for the attribute definition, Empty if it does not exist,
     *         or a Failure
     */
    def openDefinition(path : CfsAbsolutePath, principal : Principal, options : CfsOpenOptions) : Box[AttrDef] = {
        Cfs open (path, principal, options) match {
            case Full(adef : AttrDef) ⇒ Full(adef)
            case Full(other) ⇒
                other close ()
                Failure(s"$path is not an attribute")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Get the unique attribute id for a named attribute, which is the resource id of the
     * attribute definition file. The attribute is named by a path, which can be absolute,
     * or is otherwise taken as relative to "/System/Attributes".
     *
     * @param path the attribute definition file path
     * @return the boxed id of the attribute, Empty if the attribute does not exist,
     *         or a Failure
     */
    def getAttributeId(path : String) : Box[AttributeId] = {
        getAttributePath (path) flatMap { apath ⇒
            openDefinition (apath, SystemPrincipal, CfsOpenOptions.Default) map { adef ⇒
                val id = AttributeId(adef.getResourceId.id)
                adef close ()
                id
            }
        }
    }

    /**
     * Get the attribute id and type of a named attribute.
     *
     * @param path the attribute definition file path
     * @return the boxed (id, type) for the attribute, Empty if the attribute does not
     *         exist, or a Failure
     */
    def getAttributeIdAndType(path : String) : Box[(AttributeId, AttributeType)] = {
        getAttributePath (path) flatMap { apath ⇒
            openDefinition (apath, SystemPrincipal, CfsOpenOptions.Default) flatMap { adef ⇒
                val result = adef.getDataUnchecked map { ainfo ⇒
                    (AttributeId(adef.getResourceId.id), ainfo.atype)
                }
                adef close ()
                result
            }
        }
    }

    def getAttributeType(id : AttributeId) : Box[AttributeType] = {
        Cfs open (CfsVFileId(id.id), SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(adef : AttrDef) ⇒
                val result = adef.getData map (_.atype)
                adef close ()
                result
            case Full(other) ⇒
                other close ()
                Failure(s"${id.id} is not an attribute id")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    override val canUnlinkObject: RightsCheck = AnyRightsCheck("remove_attribute")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    override val canCreateObject: RightsCheck = AnyRightsCheck("define_attribute")

    val canInspectAttribute = AnyRightsCheck("inspect_attribute_def")
    val canModifyAttribute = AnyRightsCheck("modify_attribute_def")
}
