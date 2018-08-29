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
package choice.script.lua

import choice.access.Principal
import choice.fs._
import choice.script.{CfsAttrLib, CfsFileLib, CfsUserLib}
import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import org.luaj.vm2._
import org.luaj.vm2.compiler.LuaC
import org.luaj.vm2.lib._
import org.luaj.vm2.lib.jse.JseMathLib

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * This object is used to run a Lua script from an fschoice file.
 * The script is run under the principal associated with the specified file.
 */
object LuaLauncher {

    def run(script : CfsLuaScript, principal : Principal,
            args : Option[JArray], options : LuaRunOptions) : Box[(Int, Any)] = tryo {
        val scriptInput = CfsFiles.newInputStream(script)
        try {
            val chenv = standardGlobals(principal)
            val globals = chenv.globals
            try {
                val varargs = getArgs(args)
                val arg = LuaValue.listOf(varargs)
                arg.set("n", varargs.length)
                globals.set("arg", arg)
                // loadStream closes scriptInput
                val vascript = globals.baselib.loadStream(scriptInput, script.getName, "t", globals)
                if (vascript.isfunction(1)) {
                    (1, luaValueToAny(vascript.checkfunction(1).invoke(LuaValue.varargsOf(varargs))))
                }
                else {
                    (-1, luaValueToAny(vascript))
                }
            }
            catch {
                case ex : Exception ⇒
                    (-1, ex.getMessage)
            }
            finally {
                chenv.iolib.closeAll()
            }
        }
        finally {
            scriptInput close ()
        }
    }

    def standardGlobals(principal : Principal) : ChoiceEnviron = {
        val globals = new Globals()
        val chenv = ChoiceEnviron(principal, globals)
        val baselib = new LuaBaseLib(chenv)
        globals.load(baselib)
        val packagelib = new LuaPackageLib(chenv)
        globals.load(packagelib)
        val filelib = new LuaFileLib(chenv)
        globals.load(filelib)
        val attrlib = new LuaAttrLib(chenv)
        globals.load(attrlib)
        val userlib = new LuaUserLib(chenv)
        globals.load(userlib)
        globals.load(new Bit32Lib)
        globals.load(new TableLib)
        globals.load(new StringLib)
        globals.load(new CoroutineLib)
        globals.load(new JseMathLib())
        globals.load(chenv.iolib)
        LoadState.install(globals)
        LuaC.install(globals)
        chenv
    }

    def jsonToLuaValue(jval : JValue) : LuaValue = {
        jval match {
            case JNull ⇒ LuaValue.NIL
            case JString(s) ⇒ LuaString.valueOf(s)
            case JInt(bi) ⇒ LuaInteger.valueOf(bi.toLong)
            case JDouble(d) ⇒ LuaDouble.valueOf(d)
            case JBool(b) ⇒ LuaValue.valueOf(b)
            case JArray(list) ⇒
                val t = new LuaTable(list.length, 0)
                for ((elem, i) ← list.zipWithIndex) t set (i + 1, jsonToLuaValue(elem))
                t
            case JObject(list) ⇒
                val t = new LuaTable(0, list.length)
                for ((elem, i) ← list.zipWithIndex) t set (elem.name, jsonToLuaValue(elem.value))
                t
            case _ ⇒ LuaValue.NIL
        }
    }

    def varargsToJson(v : Varargs) : JValue = {
        def helper(rv : Varargs, jv : JValue) : JValue = {
            val vlua = rv.arg(1)
            val nrv = rv.subargs(2)
            val newjv = vlua match {
                case LuaValue.NONE ⇒ null
                case LuaValue.NIL ⇒ JNull
                case s : LuaString ⇒ JString(s.tojstring())
                case n : LuaInteger ⇒ JInt(BigInt(n.tolong()))
                case d : LuaDouble ⇒ JDouble(d.todouble())
                case b : LuaBoolean ⇒ JBool(b.toboolean())
                case t : LuaTable ⇒
                    val m = makeResultMap(t, LuaValue.NIL, Map())
                    mapToArray(m) match {
                        case Some(a) ⇒ Extraction.decompose(a)(DefaultFormats)
                        case None ⇒ Extraction.decompose(m)(DefaultFormats)
                    }
            }
            if (newjv == null) jv else helper(nrv, jv ++ newjv)
        }
        helper(v, JNothing)
    }

    def getArgs(args : Option[JArray]) : Array[LuaValue] = {
        args match {
            case Some(JArray(arglist)) ⇒
                val abuffer = new ArrayBuffer[LuaValue](arglist.length)
                for (arg ← arglist) {
                    abuffer append jsonToLuaValue(arg)
                }
                abuffer.toArray
            case None ⇒
                new Array[LuaValue](0)
        }
    }

    def luaValueToAny(v : Varargs) : Option[Any] = {
        v match {
            case table : LuaTable ⇒
                val m = makeResultMap(table, LuaValue.NIL, Map())
                mapToArray(m) match {
                    case a @ Some(_) ⇒ a
                    case None ⇒ Some(m)
                }
            case i : LuaInteger ⇒ Some(i.tolong())
            case d : LuaDouble ⇒ Some(d.todouble())
            case s : LuaString ⇒ Some(s.tojstring())
            case b : LuaBoolean ⇒ Some(b.booleanValue())
            case x if x == LuaValue.NIL ⇒ Some(null)
            case _ ⇒
                val abuf = new ArrayBuffer[Any](v.narg())
                for (i ← 1 to v.narg()) {
                    abuf.append(luaValueToAny(v.arg(i)))
                }
                Some(abuf.toArray)
        }
    }

    def mapToArray(m : Map[String, Any]) : Option[Array[Any]] = {
        def helper(keys : Iterator[String], minindex : Int, maxindex : Int) : Option[(Int, Int)] = {
            if (keys.hasNext) {
                val key = keys.next()
                try {
                    val kv = key.toInt
                    helper(keys, math.min(kv, minindex), math.max(kv, maxindex))
                }
                catch {
                    case _ : NumberFormatException ⇒ None
                }
            }
            else Some((minindex, maxindex))
        }
        val msize = m.size
        if (msize == 0) Some(Array[Any]())
        else helper(m.keys.iterator, Int.MaxValue, Int.MinValue) match {
            case Some((1, maxindex)) if maxindex == msize ⇒
                val result = new Array[Any](msize)
                for ((k, v) ← m) {
                    result(k.toInt - 1) = v
                }
                Some(result)
            case _ ⇒ None
        }
    }

    def makeResultMap(table : LuaTable, lastkey : LuaValue, rmap : Map[String, Any]) : Map[String, Any] = {
        val kv = table next lastkey
        if (kv.arg1().isnil()) rmap
        else {
            val key = kv.arg1()
            val value = kv.arg(2)
            (luaValueToAny(key), luaValueToAny(value)) match {
                case (Some(k), Some(v)) ⇒ makeResultMap(table, key, rmap + (k.toString → v))
                case _ ⇒ makeResultMap(table, key, rmap)
            }
        }
    }

    def anyToLuaValue(value : Any) : LuaValue = {
        value match {
            case Some(v) ⇒ anyToLuaValue(v)
            case v: Boolean ⇒ LuaValue.valueOf(v)
            case v: Int ⇒ LuaValue.valueOf(v)
            case v: Double ⇒ LuaValue.valueOf(v)
            case v: String ⇒ LuaValue.valueOf(v)
            case v: Long ⇒ LuaInteger.valueOf(v)
            case list : List[Any] ⇒
                val t = new LuaTable()
                list.zipWithIndex foreach {
                    case (elem, i) ⇒ t set (i+1, anyToLuaValue(elem))
                }
                t
            case a : Array[Any] ⇒
                val t = new LuaTable()
                for (i ← a.indices) {
                    t set (i+1, anyToLuaValue(a(i)))
                }
                t
            case m : Map[String @unchecked, Any @unchecked] ⇒
                val t = new LuaTable()
                for ((key, v) ← m) {
                    t set (key, anyToLuaValue(v))
                }
                t
            case path : CfsPath ⇒ LuaValue.valueOf(path.toString)
            case jval : JValue ⇒ jsonToLuaValue(jval)
            case _ ⇒ LuaValue.NIL
        }
    }

    def boxedMapToLuaTable(bmap : Box[Map[String, Any]]) : Varargs = {
        bmap match {
            case Full(map) ⇒ mapToLuaTable(map)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    def mapToLuaTable(smap : Map[String, Any]) : LuaTable = {
        val t = new LuaTable()
        smap foreach {
            case (key, value) ⇒ t set (key, anyToLuaValue(value))
        }
        t
    }

    def luaTableToList(table : LuaTable) : List[LuaValue] = {
        val builder = ListBuffer[LuaValue]()
        def helper(lastkey : LuaValue) : List[LuaValue] = {
            val kv = table next lastkey
            if (kv.arg1().isnil()) builder.toList
            else {
                val key = kv.arg1()
                if (key.isinttype()) builder.append(kv.arg(2))
                helper(key)
            }
        }
        helper(LuaValue.NIL)
    }

    def errorResult(msg : String) : Varargs = LuaValue.varargsOf(LuaValue.NIL, LuaValue.valueOf(msg))
}

/**
 * Encapsulate environmental variables for a LuaLauncher execution.
 *
 * @param originalPrincipal the original principal executing the script
 * @param globals the Lua Globals table
 */
case class ChoiceEnviron(originalPrincipal : Principal, globals : Globals) {
    type LFType = Class[_ <: LibFunction]

    private var _currentPrincipal = originalPrincipal

    lazy val cfsFileLib = new CfsFileLib(principal _)
    lazy val cfsUserLib = new CfsUserLib(principal _, cfsFileLib)
    lazy val cfsAttrLib = new CfsAttrLib(principal _, cfsFileLib)

    lazy val iolib = new FsIoLib(this)

    /**
     * Return the current principal, which can be changed when a script is running
     * as a system administrator.
     *
     * @return the current principal for server operations
     */
    def principal : Principal = _currentPrincipal

    /**
     * Set the current principal to a specified user or group. This is only permitted
     * when the original principal is a system administrator.
     *
     * @param path the path to the user or group
     * @return the new principal, boxed
     */
    def setPrincipal(path : String) : Box[Principal] = {
        if (_currentPrincipal.isSystemAdmin_?) {
            Cfs.withExistingFile(path, originalPrincipal, CfsOpenOptions.Default) {
                case uinfo: UserInfo ⇒
                    _currentPrincipal = uinfo.getSelfPrincipal
                    Full(_currentPrincipal)
                case ginfo: GroupInfo ⇒
                    _currentPrincipal = ginfo.getSelfPrincipal
                    Full(_currentPrincipal)
            }
        }
        else Failure("setPrincipal requires system administrator")
    }

    /**
     * This restores the current principal to its original value.
     *
     * @return the current principal, now set to the original principal
     */
    def restorePrincipal() : Principal = {
        _currentPrincipal = originalPrincipal
        _currentPrincipal
    }

    /**
     * Bind a set of library functions.
     * <p>
     * An array of names is provided, and the first name is bound
     * with opcode = 0, second with 1, etc.
     * @param env The environment to apply to each bound function
     * @param factory the Class to instantiate for each bound function
     * @param names array of String names, one for each function.
     * @see #bind(LuaValue, Class, String[], int)
     */
    def bind(env : LuaValue, factory : LFType, names : Array[String]) : Unit = {
        bind(env, factory, names, 0)
    }

    /**
     * Bind a set of library functions, with an offset
     * <p>
     * An array of names is provided, and the first name is bound
     * with opcode = {firstopcode}, second with firstopcode+1}, etc.
     * @param env The environment to apply to each bound function
     * @param factory the Class to instantiate for each bound function
     * @param names array of String names, one for each function.
     * @param firstopcode the first opcode to use
     * @see #bind(LuaValue, Class, String[])
     */
    def bind(env : LuaValue, factory : LFType, names : Array[String], firstopcode : Int) : Unit = {
        try {
            val constructor = factory.getConstructor(classOf[ChoiceEnviron], classOf[String], classOf[Int])
            names.zipWithIndex foreach {
                case (name, index) ⇒
                    val f = constructor.newInstance(this, name, new Integer(firstopcode + index))
                    env.set(name, f)
            }
        }
        catch {
            case e : Exception ⇒ throw new LuaError("bind failed: " + e)
        }
    }
}
