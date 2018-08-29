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
  * Created by Hep on 11/3/2016.
  */
package choice.script.js

import javax.script.{ScriptEngine, ScriptException}

import choice.access.Principal
import jdk.nashorn.api.scripting.{JSObject, ScriptObjectMirror}
import net.liftweb.common.{Empty, Failure, Full}

class JsAttrLib(private val context : JavaScriptContext, private implicit val engine : ScriptEngine) {

    private def cfsAttrLib = context.cfsAttrLib

    def principal : Principal = context.principal

    def defineAttr(path : String, atype : String, desc : String) : JSObject = {
        val result = cfsAttrLib.defineAttr(path, atype, desc)
        Launcher.boxedMapToObject(result)
    }

    def getAttributesOf(path : String) : JSObject = {
        cfsAttrLib.getAttributesOf(path) match {
            case Full(alist) ⇒
                Launcher.listForJS(alist)
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def getAttributeValuesOf(path : String, attrs : ScriptObjectMirror) : JSObject = {
        if (attrs.isArray) {
            val alist = (Range.inclusive(attrs.size()-1, 0, -1) map (attrs.getSlot(_).asInstanceOf[String])).toList
            cfsAttrLib.getAttributeValuesOf(path, alist) match {
                case Full(rlist) ⇒
                    Launcher.listForJS(rlist)
                case Empty ⇒ Launcher.emptyFailure()
                case f : Failure ⇒ Launcher.errorFailure(f)
            }
        }
        else throw new ScriptException("getAttributeValuesOf: second argument is not an array")
    }

    def setAttributeValuesOf(path : String, avalues : ScriptObjectMirror) : JSObject = {
        def helper(obj : ScriptObjectMirror) : (String, Any) = {
            (obj.getMember("name").asInstanceOf[String], obj.getMember("value"))
        }
        val alist = {
            if (avalues.isArray) {
                (Range.inclusive(avalues.size()-1, 0, -1) map { i ⇒
                    helper(avalues.getSlot(i).asInstanceOf[ScriptObjectMirror])
                }).toList
            }
            else List(helper(avalues))
        }
        cfsAttrLib.setAttributeValuesOf(path, alist) match {
            case Full(rlist) ⇒
                Launcher.listForJS(rlist)
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }
}
