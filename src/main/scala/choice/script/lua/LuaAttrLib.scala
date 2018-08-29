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
  * Lua library for access to Cfs file attributes.
  *
  * @author Howard Palmer
  * Created by Hep on 5/25/2014.
  */
package choice.script.lua

import choice.access.Principal
import net.liftweb.common._
import org.luaj.vm2._
import org.luaj.vm2.lib.{TwoArgFunction, VarArgFunction}

import scala.collection.mutable.ListBuffer

class LuaAttrLib(chenv : ChoiceEnviron) extends TwoArgFunction {

    override def call(modname : LuaValue, env : LuaValue) : LuaValue = {
        val t = new LuaTable()
        chenv.bind(t, classOf[AttrLibV], Array[String]("define", "get", "set", "getAll"))
        env.set("attrlib", t)
        env.get("package").get("loaded").set("attrlib", t)
        t
    }
}

/**
 * There is an instance of this class for each attrlib function. The name of the function
 * and its associated opcode are passed in to the constructor, which stores them in the
 * corresponding protected fields of the LibFunction base class. Each instance also
 * receives a ChoiceEnviron reference, which provides access to things such as the
 * current principal.
 *
 * @param chenv Choice environment reference
 * @param nameS the function name
 * @param opcodeS the function opcode
 */
private class AttrLibV(chenv : ChoiceEnviron,
                       nameS : String,
                       opcodeS : Int) extends VarArgFunction {
    import choice.script.lua.LuaLauncher._

    name = nameS
    opcode = opcodeS

    def principal : Principal = chenv.principal

    override def invoke(args : Varargs) : Varargs = {
        opcode match {
            case 0 ⇒ defineAttr(args.checkjstring(1), args.checkjstring(2), args.optjstring(3, ""))
            case 1 ⇒
                if (args.narg() == 1) {
                    getAttr(args.checkjstring(1))
                }
                else {
                    val lbuffer = new ListBuffer[String]()
                    for (i ← 2 to args.narg()) lbuffer.append(args.checkjstring(i))
                    getAttr(args.checkjstring(1), lbuffer.toList)
                }
            case 2 ⇒
                val path = args.checkjstring(1)
                def helper(i : Int, list : ListBuffer[(String, Any)]) : Varargs = {
                    if (i > args.narg()) setAttr(path, list.toList)
                    else {
                        args.arg(i) match {
                            case key : LuaString ⇒
                                if (i < args.narg()) {
                                    val value = luaValueToAny(args.arg(i + 1)).orNull
                                    list append ((key.tojstring(), value))
                                    helper(i + 2, list)
                                }
                                else errorResult (s"missing value for ${key.tojstring()}")
                            case pair : LuaTable ⇒
                                val key = pair get LuaValue.valueOf("name") match {
                                    case LuaValue.NIL ⇒ pair get 1
                                    case s : LuaString ⇒ s
                                    case _ ⇒ LuaValue.NIL
                                }
                                val value = pair get LuaValue.valueOf("value") match {
                                    case LuaValue.NIL ⇒ pair get 2
                                    case v : LuaValue ⇒ v
                                }
                                key match {
                                    case LuaValue.NIL ⇒ errorResult ("missing attribute name")
                                    case s : LuaString ⇒
                                        val v = luaValueToAny(value).orNull
                                        list append ((s.tojstring(), v))
                                        helper(i + 1, list)
                                    case _ ⇒ errorResult ("invalid attribute name")
                                }
                            case _ ⇒ errorResult ("invalid arguments")
                        }
                    }
                }
                helper(2, new ListBuffer[(String, Any)]())
            case _ ⇒ errorResult ("not implemented")
        }
    }

    def defineAttr(path : String, atype : String, desc : String) : Varargs = {
        val result = chenv.cfsAttrLib.defineAttr(path, atype, desc)
        boxedMapToLuaTable(result)
    }

    def getAttr(path : String) : Varargs = {
        val result = chenv.cfsAttrLib.getAttributesOf(path) flatMap { alist ⇒
            Full(getAttr(path, alist))
        }
        result match {
            case Full(va) ⇒ va
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    def getAttr(path : String, attrs : List[String]) : Varargs = {
        chenv.cfsAttrLib.getAttributeValuesOf(path, attrs) match {
            case Full(alist) ⇒
                val t = new LuaTable()
                alist.zipWithIndex foreach {
                    case (amap, i) ⇒
                        t set (i+1, mapToLuaTable(amap))
                }
                t
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.msg)
        }
    }

    def setAttr(path : String, attrs : List[(String, Any)]) : Varargs = {
        chenv.cfsAttrLib.setAttributeValuesOf(path, attrs) match {
            case Full(alist) ⇒
                val t = new LuaTable()
                alist.zipWithIndex foreach {
                    case (amap, i) ⇒
                        t set (i+1, mapToLuaTable(amap))
                }
                t
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }
}
