/**
  * Copyright © 2014-2018 The Board of Trustees of The Leland Stanford Junior University.
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

import java.io.IOException

import choice.access.Principal
import org.luaj.vm2._
import org.luaj.vm2.lib.PackageLib.loadlib
import org.luaj.vm2.lib.{OneArgFunction, PackageLib, VarArgFunction}

/**
 * Lua PackageLib extension for Cfs.
 * Created by Hep on 5/20/2014.
 */
class LuaPackageLib(chenv : ChoiceEnviron) extends PackageLib {

    import org.luaj.vm2.lib.PackageLib.DEFAULT_LUA_PATH

    private final val _LOADED = LuaString.valueOf("loaded")
    private final val _LOADLIB = LuaString.valueOf("loadlib")
    private final val _PRELOAD = LuaString.valueOf("preload")
    private final val _PATH = LuaString.valueOf("path")
    private final val _SEARCHPATH = LuaString.valueOf("searchpath")
    private final val _SEARCHERS = LuaString.valueOf("searchers")

    // Define getter and setter for a proxy for the base class field, 'globals',
    // which is otherwise inaccessible.
    private def _globals : Globals = {
        val f = classOf[PackageLib].getDeclaredField("globals")
        if (!f.isAccessible) {
            f.setAccessible(true)
        }
        val v = f.get(this)
        if (v == null) null else v.asInstanceOf[Globals]
    }

    private def _globals_=(g : Globals) : Unit = {
        val f = classOf[PackageLib].getDeclaredField("globals")
        if (!f.isAccessible) {
            f.setAccessible(true)
        }
        f.set(this, g)
    }

    // Define getter and setter for a proxy for the base class field, 'package_',
    // which is otherwise inaccessible.
    private def _package : LuaTable = {
        val f = classOf[PackageLib].getDeclaredField("package_")
        if (!f.isAccessible) {
            f.setAccessible(true)
        }
        val v = f.get(this)
        if (v == null) null else v.asInstanceOf[LuaTable]
    }

    private def _package_=(t : LuaTable) : Unit = {
        val f = classOf[PackageLib].getDeclaredField("package_")
        if (!f.isAccessible) {
            f.setAccessible(true)
        }
        f.set(this, t)
    }

    private var _preload_searcher : PreloadSearcher = _

    def principal : Principal = chenv.principal

    override def call(modname: LuaValue, env: LuaValue) : LuaValue = {
        PackageLib.DEFAULT_LUA_PATH = "/library/lua/?.lua"
        _globals = env.checkglobals
        _globals.set("require", new RequireImpl)
        _package = new LuaTable
        _package.set(_LOADED, new LuaTable)
        _package.set(_PRELOAD, new LuaTable)
        _package.set(_PATH, LuaValue.valueOf(DEFAULT_LUA_PATH))
        _package.set(_LOADLIB, new loadlib)
        _package.set(_SEARCHPATH, new SearchPathImpl)
        val searchers = new LuaTable
        this._preload_searcher = new PreloadSearcher
        searchers.set(1, this._preload_searcher)
        _package.set(_SEARCHERS, searchers)
        _package.get(_LOADED).set("package", _package)
        env.set("package", _package)
        _globals.package_ = this
        env
    }

    /**
     * require (modname)
     *
     * Loads the given module. The function starts by looking into the package.loaded table
     * to determine whether modname is already loaded. If it is, then require returns the value
     * stored at package.loaded[modname]. Otherwise, it tries to find a loader for the module.
     *
     * To find a loader, require is guided by the package.searchers sequence.
     * By changing this sequence, we can change how require looks for a module.
     * The following explanation is based on the default configuration for package.searchers.
     *
     * First require queries package.preload[modname]. If it has a value, this value
     * (which should be a function) is the loader. Otherwise require searches for a Lua loader using
     * the path stored in package.path. If that also fails, it searches for a Java loader using
     * the classpath, using the public default constructor, and casting the instance to LuaFunction.
     *
     * Once a loader is found, require calls the loader with two arguments: modname and an extra value
     * dependent on how it got the loader. If the loader came from a file, this extra value is the file name.
     * If the loader is a Java instance of LuaFunction, this extra value is the environment.
     * If the loader returns any non-nil value, require assigns the returned value to package.loaded[modname].
     * If the loader does not return a non-nil value and has not assigned any value to package.loaded[modname],
     * then require assigns true to this entry.
     * In any case, require returns the final value of package.loaded[modname].
     *
     * If there is any error loading or running the module, or if it cannot find any loader for the module,
     * then require raises an error.
     */
    class RequireImpl extends OneArgFunction {
        import choice.script.lua.LuaPackageLib._SENTINEL
        import org.luaj.vm2.LuaValue.error

        override def call(arg: LuaValue): LuaValue = {
            val name = arg.checkstring
            val loaded = _package get _LOADED
            val loaded_already = loaded get name
            if (loaded_already.toboolean) {
                if (loaded_already == _SENTINEL) error("loop or previous error loading module '" + name + "'")
                else loaded_already
            }
            else {
                val tbl = _package.get(_SEARCHERS).checktable
                val sb = new StringBuffer
                def helper(i : Int) : LuaValue = {
                    val searcher = tbl get i
                    if (searcher.isnil()) {
                        error("module '" + name + "' not found: " + name + sb)
                    }
                    else {
                        val loader = searcher invoke name
                        if (loader isfunction 1) {
                            // Set the sentinel in case the loader throws an error, and also
                            // to detect whether the loader changes it
                            loaded set (name, _SENTINEL)
                            val loader_ret = loader.arg1 call (name, loader arg 2)
                            if (loader_ret.isnil) {
                                if ((loaded get name) == _SENTINEL) {
                                    // Loader returned Nil and didn't set the loaded entry
                                    loaded set (name, LuaValue.TRUE)
                                }
                            }
                            else loaded set (name, loader_ret)
                            loaded get name
                        }
                        else if (loader isstring 1) {
                            sb append (loader tojstring 1)
                            helper(i + 1)
                        }
                        else helper(i + 1)
                    }
                }
                helper(1)
            }
        }
    }

    class PreloadSearcher extends VarArgFunction {
        override def invoke(args: Varargs): Varargs = {
            val name = args checkstring 1
            val preload = _package get _PRELOAD
            val result = if (preload.isnil()) preload else preload get name
            if (result.isnil) LuaValue.valueOf(s"\n\tno field package.preload['$name']") else result
        }
    }

    class SearchPathImpl extends VarArgFunction {
        import choice.script.lua.LuaPackageLib.FILE_SEP

        override def invoke(args: Varargs): Varargs = {
            val name = args checkjstring 1
            val path = args checkjstring 2
            val sep = (args optjstring (3, ".")) charAt 0
            val rep = (args optjstring (4, FILE_SEP)) charAt 0
            val plen = path.length
            def helper(name : String, index : Int, sb : StringBuffer) : Varargs = {
                val bs = index + 1
                if (bs < plen) {
                    val es = {
                        val k = path indexOf(';', bs)
                        if (k < 0) plen else k
                    }
                    val template = path substring(bs, es)
                    val q = template indexOf '?'
                    val filename =
                        if (q < 0) template
                        else template.substring(0, q) + name + template.substring(q + 1)
                    val instream = _globals.finder findResource filename
                    if (instream == null) {
                        val nnsb = if (sb == null) new StringBuffer() else sb
                        nnsb append s"\n\t$filename"
                        helper(name, es, nnsb)
                    }
                    else {
                        try {
                            instream close ()
                        }
                        catch {
                            case _ : IOException ⇒
                        }
                        LuaValue.valueOf(filename)
                    }
                }
                else LuaValue.varargsOf(LuaValue.NIL, LuaString.valueOf(sb.toString))
            }
            helper(name replace (sep, rep), -1, null)
        }
    }

}

object LuaPackageLib {
    val _SENTINEL : LuaString = LuaString.valueOf("\u0001")
    val FILE_SEP : String = "/"
}
