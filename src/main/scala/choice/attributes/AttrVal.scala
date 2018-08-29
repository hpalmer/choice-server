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
  * Created by Hep on 2/26/2014
  */
package choice.attributes

import net.liftweb.mapper._
import choice.model.{Resource, ResourceId}
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.json._
import net.liftweb.common.Full
import net.liftweb.json.JsonAST.{JValue, JNothing, JNull}
import choice.access.Principal
import choice.fs.Cfs
import choice.fs.vfs.AttributeId
import choice.fs.vfs.AttributeType.AttributeType
import choice.fs.vfs.AttributeType

/**
 * User attribute value.
 */
class AttrVal extends LongKeyedMapper[AttrVal] with IdPK {
    def getSingleton : AttrVal.type = AttrVal

    /** Attribute definition */
    object adef extends MappedLongForeignKey(this, Resource)

    /** User to which this value is assigned */
    object target extends MappedLongForeignKey(this, Resource)

    /** The attribute type (which should match the attribute definition type) */
    object atype extends MappedEnum(this, AttributeType)

    /** Time of last change to this value */
    object stime extends MappedLong(this)

    /** Principal who set the value */
    object setter extends MappedLongForeignKey(this, Resource)

    /** The attribute value */
    object value extends MappedString(this, 512)

    def getValue : Box[Any] = {
        val strval = value.get
        atype.get match {
            case AttributeType.STRING ⇒ Full(strval)
            case AttributeType.NUMBER ⇒
                val tval = strval.trim
                tryo(if (tval contains '.') tval.toDouble else tval.toLong)
            case AttributeType.BOOLEAN ⇒ asBoolean (strval.trim) match {
                case full @ Full(_) ⇒ full
                case _ : EmptyBox ⇒ Failure(s"invalid boolean value: $strval")
            }
            case AttributeType.JSON ⇒ tryo(JsonParser.parse(strval.trim))
            case AttributeType.FILE ⇒
                Cfs.withValidPath (strval) { cfspath ⇒ Full(cfspath.toString) }
        }
    }


    def asMap(principal : Principal) : Map[String, Any] = {
        val attrval = getValue openOr None
        Map("value" → attrval, "stime" → stime.get)
    }
}

object AttrVal extends AttrVal with LongKeyedMetaMapper[AttrVal] {
    implicit val formats : DefaultFormats.type = DefaultFormats

    def get(attribute : AttributeId, resource : ResourceId) : Box[AttrVal] = {
        AttrVal find (By(AttrVal.target, resource.id), By(AttrVal.adef, attribute))
    }

    def getAll(resource : ResourceId) : List[AttrVal] = {
        AttrVal findAll By(AttrVal.target, resource)
    }

    def deleteAll(resource : ResourceId) : Boolean = {
        AttrVal bulkDelete_!! By(AttrVal.target, resource)
    }

    def hasValues(resource : ResourceId) : Boolean = {
        AttrVal find By(AttrVal.target, resource) map (_ ⇒ true) openOr false
    }

    def getValue(attribute : AttributeId, resource : ResourceId) : Box[Any] = {
        get (attribute, resource) flatMap (_.getValue)
    }

    def setValue(attribute : AttributeId, atype : AttributeType,
                 resource : ResourceId, value : Any, principal : ResourceId) : Box[AttrVal] = {
        // If the value is empty, remove any existing AttrVal
        if (isEmptyValue_? (value)) {
            get (attribute, resource) match {
                case Full(avalue) ⇒
                    avalue.delete_!
                    Empty
                case e : EmptyBox ⇒ e
            }
        }
        else {
            val avaluebox = get (attribute, resource) match {
                case Full(avalue) ⇒
                    if (avalue.atype.get == atype) Full(avalue)
                    else Failure(s"setValue: wrong attribute type $atype")
                case Empty ⇒ Full(AttrVal.create.adef(attribute).atype(atype).target(resource))
                case f : Failure ⇒ f
            }
            avaluebox flatMap { avalue ⇒
                valueToString (atype, value) flatMap { newval ⇒
                    tryo(avalue.value(newval).setter(principal).stime(millis).saveMe())
                }
            }
        }
    }

    def isEmptyValue_?(value : Any) : Boolean = value match {
        case null | JNull | JNothing | None | Empty | Nil | JArray(Nil) ⇒ true
        case _ ⇒ false
    }

    def valueToString(atype : AttributeType, value : Any) : Box[String] = {
        val decodedValue = {
            value match {
                case jv : JValue ⇒
                    if (atype == AttributeType.JSON) jv
                    else value match {
                        case JNull | JNothing ⇒ null
                        case JBool(b) ⇒ b
                        case JInt(i) ⇒ i.toLong
                        case JDouble(d) ⇒ d
                        case JString(s) ⇒ s
                        case JField(_, v) ⇒ valueToString(atype, v)
                        case JArray(list) ⇒ list.headOption.map(valueToString(atype, _)).orNull
                    }
                case _ ⇒ value
            }
        }
        atype match {
            case AttributeType.STRING ⇒ Full(decodedValue.toString)
            case AttributeType.NUMBER ⇒
                tryo(decodedValue.asInstanceOf[Number]) map (_.toString)
            case AttributeType.BOOLEAN ⇒
                decodedValue match {
                    case b : Boolean ⇒ Full(b.toString)
                    case i : Int ⇒ Full((i == 0).toString)
                    case i : Long ⇒ Full((i == 0).toString)
                    case d : Double ⇒ Full((d == 0.0).toString)
                    case s : String ⇒ asBoolean(s.trim) match {
                        case Full(b) ⇒ Full(b.toString)
                        case _ : EmptyBox ⇒ Failure("invalid boolean value: " + s)
                    }
                    case _ ⇒ Failure("invalid boolean value: " + value)
                }
            case AttributeType.JSON ⇒
                decodedValue match {
                    case jv : JValue ⇒ Full(compactRender(jv))
                    case s : String ⇒
                        tryo(JsonParser.parse(s.trim)) match {
                            case Full(jv) ⇒ Full(compactRender(jv))
                            case _ : EmptyBox ⇒ Failure("invalid JSON value: " + s)
                        }
                    case other ⇒
                        tryo(compactRender(Extraction.decompose(other)))
                }
            case AttributeType.FILE ⇒
                Cfs.withValidPath (decodedValue.toString) { cfspath ⇒ Full(cfspath.toString) }
        }
    }
}
