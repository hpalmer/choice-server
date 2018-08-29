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
package choice.fs.vfs

import net.liftweb.common.{Box, Failure, Full}
import choice.fs.vfs.AttributeType.AttributeType

/**
 * Attribute type enumeration.
 *
 * Metadata attributes are defined with one of these types.
 */
object AttributeType extends Enumeration {
    type AttributeType = Value
    val STRING : AttributeType.Value = Value(0)
    val NUMBER : AttributeType.Value = Value(1)
    val BOOLEAN : AttributeType.Value = Value(2)
    val JSON : AttributeType.Value = Value(3)
    val FILE : AttributeType.Value = Value(4)

    def convertAttributeTypeString(s : String) : Box[AttributeType] = {
        s.trim.toLowerCase match {
            case "string" ⇒ Full(AttributeType.STRING)
            case "number" ⇒ Full(AttributeType.NUMBER)
            case "boolean" ⇒ Full(AttributeType.BOOLEAN)
            case "json" ⇒ Full(AttributeType.JSON)
            case "file" ⇒ Full(AttributeType.FILE)
            case _ ⇒ Failure("invalid attribute type")
        }
    }
}

case class AttributeId(id : Long) extends AnyVal

object AttributeId {
    import language.implicitConversions

    implicit def idToLong(attid : AttributeId) : Long = attid.id
    implicit def longToId(id : Long) : AttributeId = AttributeId(id)
}

/**
 * Encapsulate information about an attribute value attached to some file. If there is
 * no value for the attribute associated with the file, the value field is null, and
 * the setter and lastset fields are meaningless.
 *
 * @param id the unique id of the attribute
 * @param atype the type of the attribute
 * @param value the value of the attribute
 * @param setter a unique id of the last principal to set the attribute value
 * @param lastset a timestamp of the last set operation
 */
case class AttributeInfo(id : AttributeId, atype : AttributeType, value : Any, setter : Long, lastset : Long) {
    def asMap : Map[String, Any] = {
        val v = value match {
            case path : VPath ⇒ path.toString
            case other ⇒ other
        }
        Map("id" → id.id, "atype" → atype.toString, "value" → v, "setter" → setter, "stime" → lastset)
    }
}

/**
 * Mixin trait for VFiles which support metadata. The implementation of metadata
 * may vary across filesystems. For a typical filesystem, one approach might be
 * to have a ".meta" folder residing in the same folder with files that have
 * metadata. The metadata for each of those files could reside in a folder in
 * .meta having the same name as the file.
 *
 * Metadata is modeled as a set of typed attributes for which a particular file
 * may have associated values. An attribute is identified by a unique numeric id,
 * but an implementation will typically provide a way to map an attribute name
 * to its id.
 *
 * The principal for metadata operations is assumed to be the principal associated
 * with the current file.
 */
trait VfsMeta {

    /**
     * Check whether the file has any associated metadata attributes. This may be
     * subject to access control, which could cause Failure to be returned.
     *
     * @return a boxed value of true if the file has attributes, or Failure if the
     *         determination cannot be made
     */
    def hasAttributes : Box[Boolean]

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
    def listAttributes : Box[List[AttributeInfo]]

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
    def getAttribute(id : AttributeId, atype : Option[AttributeType]) : Box[AttributeInfo]

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
    def setAttribute(id : AttributeId, atype : Option[AttributeType], value : Any) : Box[AttributeInfo]
}
