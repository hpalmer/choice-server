/**
  * Copyright © 2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * Created by Hep on 11/1/2016.
  */
package choice.script

import choice.access.Principal
import choice.attributes.{AttrDef, AttrDefCreateOptions, AttrVal}
import choice.fs._
import choice.fs.vfs.AttributeType
import net.liftweb.common._

class CfsAttrLib(principal : () ⇒ Principal, cfsFileLib : CfsFileLib) {

    /**
      * Check whether a specified attribute definition exists, and create it if not. If
      * the given path is relative, it is interpreted as relative to "/System/Attributes".
      * The returned map contains a "status" field, which has a value of 0 if the
      * attribute definition already exists, or a value of 1 if it was just created. If
      * the attribute definition already exists, the specified type must match the type
      * in the existing definition. The returned map has general file metadata information
      * for the attribute definition file.
      *
      * @param path absolute or relative path of the attribute definition
      * @param atype the attribute type
      * @param desc a description of the attribute
      * @return a boxed map of file information plus status
      */
    def defineAttr(path : String, atype : String, desc : String) : Box[Map[String, Any]] = {
        AttributeType convertAttributeTypeString atype flatMap { atypeval ⇒
            AttrDef getAttributePath path flatMap { apath ⇒
                AttrDef openDefinition (apath, principal(), CfsOpenOptions.Default) match {
                    case Full(adef) ⇒
                        val result = adef.getData flatMap { ainfo ⇒
                            if (ainfo.atype == atypeval) Full(adef.asMap + ("status" → 0))
                            else Failure(s"attribute already exists with type ${ainfo.atype}")
                        }
                        adef close ()
                        result
                    case Empty ⇒
                        val options = AttrDefCreateOptions(atype = atypeval, description = Some(desc))
                        Cfs create (apath, principal(), AttrDef.getMimeType, options) match {
                            case Full(adef : AttrDef) ⇒
                                val result = adef.asMap + ("status" → 1)
                                adef close ()
                                Full(result)
                            case Full(other) ⇒
                                other close ()
                                Failure(s"system error: wrong file type")
                            case e : EmptyBox ⇒ e ?~! s"failed to create attribute file: $apath"
                        }
                    case f : Failure ⇒ f ?~! s"$path may already exist but is not accessible"
                }
            }
        }
    }

    /**
      * For all the attribute values associated with a specified file, return the file paths
      * of the attribute definitions.
      *
      * @param path the absolute or relative path of some file
      * @return a boxed list of paths to the attribute definitions which have values assigned
      *         for the specified file
      */
    def getAttributesOf(path : String) : Box[List[String]] = {
        cfsFileLib.getAbsolutePath(path) flatMap { abspath ⇒
            getAttributesOf(abspath)
        }
    }

    def getAttributesOf(path : CfsAbsolutePath) : Box[List[String]] = {
        Cfs.withExistingFile(path, principal(), CfsOpenOptions.Default) {
            case cfsfile : CfsFile ⇒
                val allAttrIds = AttrVal getAll cfsfile.getResourceId map { aval ⇒
                    aval.adef.get
                }
                val apaths = allAttrIds flatMap { attid ⇒
                    Cfs open(CfsVFileId(attid), principal(), CfsOpenOptions.Default) match {
                        case Full(adeffile : AttrDef) ⇒
                            val apath = adeffile.getPath.toString
                            adeffile close()
                            List(apath)
                        case Full(other) ⇒
                            other close()
                            Nil
                        case _ : EmptyBox ⇒ Nil
                    }
                }
                Full(apaths)
        }
    }

    def getAttributeValuesOf(path : String, attrs : List[String]) : Box[List[Map[String, Any]]] = {
        cfsFileLib.getAbsolutePath(path) flatMap { abspath ⇒
            getAttributeValuesOf(abspath, attrs)
        }
    }

    def getAttributeValuesOf(path : CfsAbsolutePath, attrs : List[String]) : Box[List[Map[String, Any]]] = {
        Cfs.withExistingFile(path, principal(), CfsOpenOptions.Default) {
            case cfsfile : CfsFile ⇒
                val maplist = attrs map { attrpath ⇒
                    var amap : List[(String, Any)] = List("name" → attrpath)
                    AttrDef getAttributeIdAndType attrpath match {
                        case Full((id, atype)) ⇒
                            amap = ("id" → id.id) :: ("type" → atype.toString) :: amap
                            cfsfile getAttribute (id, None) match {
                                case Full(ainfo) ⇒
                                    // If the setter resource id is not zero, there actually is a value.
                                    if (ainfo.setter != 0L) {
                                        amap = ("value" → ainfo.value) :: ("lastset" → ainfo.lastset) :: amap
                                        Cfs open(CfsVFileId(ainfo.setter), principal(), CfsOpenOptions.Default) match {
                                            case Full(uinfo : UserInfo) ⇒
                                                amap = ("setter" → uinfo.getPath.toString) :: amap
                                                uinfo close ()
                                            case Full(other) ⇒ other close ()
                                            case _ : EmptyBox ⇒
                                        }
                                    }
                                case e : EmptyBox ⇒
                                    // This indicates some kind of error, not the lack of a value
                                    // for the attribute.
                                    amap = ("error" → (e ?~ "Empty").messageChain) :: amap
                            }
                        case e : EmptyBox ⇒
                            amap = ("error" → (e ?~ "Empty").messageChain) :: amap
                    }
                    Map(amap : _*)
                }
                Full(maplist)
        }
    }

    def setAttributeValuesOf(path : String, avalues : List[(String, Any)]) : Box[List[Map[String, Any]]] = {
        cfsFileLib.getAbsolutePath(path) flatMap { abspath ⇒
            setAttributeValuesOf(abspath, avalues)
        }
    }

    def setAttributeValuesOf(path : CfsAbsolutePath, avalues : List[(String, Any)]) : Box[List[Map[String, Any]]] = {
        Cfs.withExistingFile(path, principal(), CfsOpenOptions.Default) {
            case cfsfile : CfsFile ⇒
                val maplist = avalues map {
                    case (apath, v) ⇒
                        var amap : List[(String, Any)] = ("name" → apath) :: Nil
                        AttrDef getAttributeId apath match {
                            case Full(id) ⇒
                                amap = ("id" → id.id) :: amap
                                cfsfile setAttribute (id, None, v) match {
                                    case Full(ainfo) ⇒
                                        amap = ("type" → ainfo.atype.toString) :: ("value" → ainfo.value) ::
                                            ("lastset" → ainfo.lastset) :: amap
                                        Cfs open (CfsVFileId(ainfo.setter), principal(), CfsOpenOptions.Default) match {
                                            case Full(uinfo : UserInfo) ⇒
                                                amap = ("setter" → uinfo.getPath.toString) :: amap
                                                uinfo close()
                                            case Full(other) ⇒ other close()
                                            case _ : EmptyBox ⇒
                                        }
                                    case e : EmptyBox ⇒
                                        amap = ("type" → null) :: ("value" → null) ::
                                            ("error" → (e ?~ "Empty").messageChain) :: amap
                                }
                            case e : EmptyBox ⇒
                                amap = ("error" → (e ?~ "Empty").messageChain) :: amap
                        }
                        Map(amap : _*)
                }
                Full(maplist)
        }
    }
}
