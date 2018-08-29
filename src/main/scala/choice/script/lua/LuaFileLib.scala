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
  * Lua access to Cfs file operations.
  *
  * @author Howard Palmer
  * Created by Hep on 9/25/2014.
  */
package choice.script.lua

import choice.access.Principal
import choice.fs._
import choice.script.lua.LuaLauncher._
import net.liftweb.common.{Empty, Failure, Full}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{JsonAST, JsonParser}
import net.liftweb.util.Helpers._
import org.luaj.vm2._
import org.luaj.vm2.lib.{TwoArgFunction, VarArgFunction}

import scala.collection.mutable.ListBuffer

object FileLibV extends Enumeration {
    val isvalidpath : Value = Value(0)
    val isvalidfilename : Value = Value
    val isabsolutepath : Value = Value
    val isrelativepath : Value = Value
    val getcwd : Value = Value
    val lookup : Value = Value
    val lookupbyid : Value = Value
    val list : Value = Value
    val mkdir : Value = Value
    val rm : Value = Value
    val cd : Value = Value
    val pushd : Value = Value
    val popd : Value = Value
    val writejson : Value = Value
    val readjson : Value = Value
    val readsession : Value = Value
    val setprincipal : Value = Value
    val restoreprincipal : Value = Value
    val create : Value = Value
    val mail : Value = Value

    val names : Array[String] = this.values.toArray.map(_.toString)
}

class LuaFileLib(chenv : ChoiceEnviron) extends TwoArgFunction {

    override def call(modname : LuaValue, env : LuaValue) : LuaValue = {
        val t = new LuaTable()
        chenv.bind(t, classOf[FileLibV], FileLibV.names)
        env.set("filelib", t)
        env.get("package").get("loaded").set("filelib", t)
        t
    }
}

class FileLibV(chenv : ChoiceEnviron, nameS : String, opcodeS : Int) extends VarArgFunction {

    name = nameS
    opcode = opcodeS

    private def cfsFileLib = chenv.cfsFileLib

    def principal : Principal = chenv.principal

    override def invoke(args : Varargs) : Varargs = {
        import choice.script.lua.FileLibV._
        FileLibV(opcode) match {
            case `isvalidpath` ⇒
                val path = args.checkjstring(1)
                anyToLuaValue(cfsFileLib.isValidPath(path))
            case `isvalidfilename` ⇒
                val name = args.checkjstring(1)
                anyToLuaValue(cfsFileLib.isValidFilename(name))
            case `isabsolutepath` ⇒
                val path = args.checkjstring(1)
                anyToLuaValue(cfsFileLib.isAbsolutePath(path))
            case `isrelativepath` ⇒
                val path = args.checkjstring(1)
                anyToLuaValue(cfsFileLib.isRelativePath(path))
            case `getcwd` ⇒ anyToLuaValue(cfsFileLib.getcwd())
            case `lookup` ⇒
                /**
                 * lookup takes one argument which is either the file path or the file id.
                 *
                 * It returns a table containing properties of the indicated file, which
                 * will vary somewhat on the file type.
                 */
                args.arg(1) match {
                    case n : LuaInteger ⇒ _lookup(n.tolong())
                    case s : LuaString ⇒ _lookup(s.tojstring())
                    case _ ⇒ errorResult("invalid file specification")
                }
            case `list` ⇒
                /**
                 * list takes a file path argument, which should be the path to a folder
                 * or folder-like file, and an optional second argument specifying a
                 * MIME type string to which the listing is limited.
                 *
                 * Currently the listing includes entries for "." and "..", which are
                 * not subject to any MIME type filter.
                 *
                 * It returns an array-like table containing a table for each file in the
                 * listing, each containing lookup properties for the file.
                 */
                val path = args.checkjstring(1)
                val mimetype = args.arg(2) match {
                    case s : LuaString ⇒ Some(s.tojstring())
                    case _ ⇒ None
                }
                _list(path, mimetype)
            case `mkdir` ⇒
                /**
                 * mkdir takes a file path argument, and an optional boolean argument
                 * indicating whether the operation should be recursive. If it is not
                 * recursive (the default), all folders in the path except the last
                 * must already exist.
                 *
                 * It returns a table containing lookup properties for the created folder.
                 */
                val path = args.checkjstring(1)
                val recursive = args.arg(2) match {
                    case b : LuaBoolean ⇒ b.toboolean()
                    case _ ⇒ false
                }
                _mkdir(path, recursive = recursive)
            case `rm` ⇒
                /**
                 * rm takes a file path argument of the file to be removed, and an optional
                 * boolean argument indicating whether the operation is to be recursive.
                 * If the operation is not recursive (the default), the indicated file
                 * must be a non-folder-like file, or if folder-like, must be empty.
                 *
                 * It returns a boolean value, which is true if there were no other links
                 * remaining for the file, in which case the file is actually gone.
                 */
                val path = args.checkjstring(1)
                val recursive = args.arg(2) match {
                    case b : LuaBoolean ⇒ b.toboolean()
                    case _ ⇒ false
                }
                _rm(path, recursive = recursive)
            case `cd` ⇒
                val path = args.checkjstring(1)
                cfsFileLib.cd(path) match {
                    case Full(abspath) ⇒ LuaValue.valueOf(abspath.toString)
                    case Empty ⇒ errorResult("Empty result")
                    case f : Failure ⇒ errorResult(f.messageChain)
                }
            case `pushd` ⇒
                val path = args.checkjstring(1)
                cfsFileLib.pushd(path) match {
                    case Full(abspath) ⇒ LuaValue.valueOf(abspath.toString)
                    case Empty ⇒ errorResult("Empty result")
                    case f : Failure ⇒ errorResult(f.messageChain)
                }
            case `writejson` ⇒
                val path = args.checkjstring(1)
                val data = args.subargs(2)
                val jsondata = varargsToJson(data)
                _writejson(path, jsondata)
            case `readjson` ⇒
                val path = args.checkjstring(1)
                _readjson(path)
            case `readsession` ⇒
                val path = args.checkjstring(1)
                _readsession(path)
            case `setprincipal` ⇒
                val path = args.checkjstring(1)
                _setprincipal(path)
            case `restoreprincipal` ⇒
                _restoreprincipal()
            case `create` ⇒
                val path = args.checkjstring(1)
                val mimetype = args.checkjstring(2)
                _create(path, mimetype)
            case `mail` ⇒
                val message = args.checktable(1)
                val mailer = if (args.narg() > 1) Some(args.checkjstring(2)) else None
                _mail(message, mailer)
        }
    }

    private def _lookup(path : String) : Varargs = {
        val result = cfsFileLib.lookup(path)
        boxedMapToLuaTable(result)
    }

    private def _lookup(id : Long) : Varargs = {
        val result = cfsFileLib.lookupById(id)
        boxedMapToLuaTable(result)
    }

    private def _list(path : String, mimetype : Option[String]) : Varargs = {
        val maps = cfsFileLib.list(path, mimetype)
        maps match {
            case Full(list) ⇒
                val t = new LuaTable(list.size, 0)
                list.zipWithIndex foreach {
                    case (m, i) ⇒ t.set(i+1, mapToLuaTable(m))
                }
                t
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    private def _mkdir(path : String, recursive : Boolean) : Varargs = {
        val result = cfsFileLib.mkdir(path, recursive)
        boxedMapToLuaTable(result)
    }

    private def _rm(path : String, recursive : Boolean) : Varargs = {
        val result = cfsFileLib.rm(path, recursive)
        result match {
            case Full(b) ⇒ LuaValue.valueOf(b)
            case Empty ⇒ LuaValue.varargsOf(LuaValue.NIL, LuaValue.valueOf("Empty result"))
            case f : Failure ⇒ LuaValue.varargsOf(LuaValue.NIL, LuaValue.valueOf(f.messageChain))
        }
    }

    private def _writejson(path : String, json : JValue) : Varargs = {
        val jsonstr = JsonAST.compactRender(json)
        val fmap = cfsFileLib.writeJSON(path, jsonstr)
        boxedMapToLuaTable(fmap)
    }

    private def _readjson(path : String) : Varargs = {
        val jval = cfsFileLib.readJSON(path) flatMap { json ⇒
            tryo(JsonParser.parse(json))
        }
        jval match {
            case Full(jv) ⇒ jsonToLuaValue(jv)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    private def _readsession(path : String) : Varargs = {
        boxedMapToLuaTable(cfsFileLib.readSession(path))
    }

    private def _setprincipal(path : String) : Varargs = {
        chenv.setPrincipal(path) match {
            case Full(_) ⇒ LuaValue.valueOf(true)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    private def _restoreprincipal() : Varargs = {
        chenv.restorePrincipal()
        val p = chenv.principal
        mapToLuaTable(p.asMap)
    }

    private def _create(path : String, mimetype : String) : Varargs = {
        val resultbox = cfsFileLib.create(path, mimetype)
        resultbox match {
            case Full(finfo) ⇒ mapToLuaTable(finfo + (("status", 1)))
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    def _mail(message : LuaTable, mailer : Option[String]) : Varargs = {
        def listHelper(table : LuaTable, key : String) : List[String] = {
            val lualist = table.get(key).opttable(new LuaTable())
            if (lualist.length() == 0) Nil
            else {
                val builder = new ListBuffer[String]()
                (0 until lualist.length()).foreach { i ⇒
                    val vargs = lualist.inext(LuaInteger.valueOf(i))
                    if (vargs != LuaValue.NONE) {
                        val addr = vargs.checkjstring(2).trim()
                        if (addr != "") {
                            builder.append(addr)
                        }
                    }
                }
                builder.toList
            }
        }
        val from = {
            val luaval = message.get("from")
            if (luaval.isnil()) None else Some(luaval.checkjstring())
        }
        val subject = {
            val luaval = message.get("subject")
            if (luaval.isnil()) None else Some(luaval.checkjstring())
        }
        val headers : Option[Map[String, String]] = {
            val luaval = message.get("headers")
            if (luaval.isnil()) None
            else {
                val hmap = makeResultMap(luaval.checktable(), LuaValue.NIL, Map[String, Any]()).map {
                    case (key, v : String) ⇒ (key, v)
                    case (key, v) ⇒ (key, v.toString)
                }
                Some(hmap)
            }
        }
        val html = {
            val luaval = message.get("html")
            !luaval.isnil() && luaval.checkboolean()
        }
        val content = {
            val luaval = message.get("content")
            if (luaval.isnil()) "" else luaval.checkjstring()
        }
        val msg = MailerMessage(from, listHelper(message, "to"),
                                listHelper(message, "cc"), listHelper(message, "bcc"),
                                subject, headers, html, content)
        cfsFileLib.sendMail(msg, mailer) match {
            case Full(ok) ⇒ LuaValue.valueOf(ok)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }
}
