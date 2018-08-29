/**
  * Copyright © 2016-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Launcher for JavaScript script files.
  *
  * @author Howard Palmer
  * Created by Hep on 10/5/2016.
  */
package choice.script.js

import java.io.{BufferedReader, BufferedWriter, StringWriter}
import javax.script.{ScriptContext, ScriptEngine, ScriptException}

import choice.access.Principal
import choice.fs._
import choice.script._
import jdk.nashorn.api.scripting.{JSObject, NashornScriptEngine, NashornScriptEngineFactory, ScriptObjectMirror}
import net.liftweb.common._
import net.liftweb.http.{Req, S}
import net.liftweb.json._
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers.tryo

import scala.collection.mutable


/**
  * Top-level object for JavaScript script execution.
  *
  * This creates an instance of the Nashorn script engine, and uses it to execute the
  * script in a node.js/CommonJS-like module environment. This provides for loading
  * dependencies via a JavaScript require() function, including some internal modules.
  *
  * When the original principal is a system administrator, the script is allowed to
  * set the current principal to any user or group. This provides a way to have scripts
  * which can perform operations on behalf of any user.
  *
  * @param options run options
  * @param originalPrincipal the original principal responsible for running the script
  * @param script the input source for reading the script
  * @param scriptPath the full path to the script file, if any, or else some name
  * @param args arguments for the script, if any
  * @param paths optional sequence of paths to be used in locating required dependencies
  * @param stdin optional input source for the script
  * @param stdout optional output target for the script
  */
final case class JavaScriptContext(options : JsRunOptions, originalPrincipal : Principal,
                                   script : BufferedReader, scriptPath : String,
                                   args : Option[JArray], paths : Option[Array[String]],
                                   stdin : Option[BufferedReader], stdout : Option[BufferedWriter])
        extends Runnable {

    import JavaScriptContext.moduleCreationFunction

    private var _currentPrincipal = originalPrincipal

    val reqBox : Box[Req] = S.request

    private val engine = (new NashornScriptEngineFactory)
                                    .getScriptEngine("--no-java", "--language=es6").asInstanceOf[NashornScriptEngine]

    private val objectConstructor = engine.eval("Object").asInstanceOf[ScriptObjectMirror]

//    private lazy val jsonConstructor = engine.eval("JSON").asInstanceOf[ScriptObjectMirror]
//    private lazy val errorConstructor = engine.eval("Error").asInstanceOf[ScriptObjectMirror]

    /**
      * This is a cache of modules that have been or are being loaded. The moduleBuilder script adds
      * new modules as they are created. It also exports the cache to modules as require.cache.
      */
    val moduleCache : ScriptObjectMirror = objectConstructor.newObject().asInstanceOf[ScriptObjectMirror]

    /**
      * Track the internal modules that have been loaded, in case a script tries to play
      * games with moduleCache.
      */
    private val internalModules : mutable.Set[String] = mutable.Set()

    /**
      * A compiled function which can be used to create a module for a script. The function
      * returns an array containing the arguments for the wrapped script.
      */
    private lazy val moduleBuilder = engine.eval(moduleCreationFunction).asInstanceOf[ScriptObjectMirror]

    private[js] lazy val cfsFileLib : CfsFileLib = new CfsFileLib(principal _)
    private[js] lazy val choiceletLib : ChoiceletLib = ChoiceletLib(principal _)
    private[js] lazy val cfsUserLib : CfsUserLib = new CfsUserLib(principal _, cfsFileLib)
    private[js] lazy val cfsAttrLib : CfsAttrLib = new CfsAttrLib(principal _, cfsFileLib)
    private[js] lazy val httpLib : HttpLib = reqBox match {
        case Full(req) ⇒ new HttpLib(req, principal _)
        case _ : EmptyBox ⇒ null
    }

    private[js] def getEngine : NashornScriptEngine = engine

    /**
      * Modules are pushed on this stack when they are created, and popped when
      * they have finished loading. The main script's module will remain on the
      * stack (at index 0) until it completes.
      */
    private val moduleStack = mutable.ArrayStack[ScriptObjectMirror]()

    /**
      * References to internal modules, filled in as they are required.
      */
    private var jsFileLib : JsFileLib = _
    private var jsUserLib : JsUserLib = _
    private var jsAttrLib : JsAttrLib = _
    private var jsHttpLib : JsHttpLib = _
    private var jsChoiceletLib : JsChoiceletLib = _

    /**
      * Return value from the main script.
      */
    private var _scriptResult : (Int, Any) = _

    def scriptResult : (Int, Any) = _scriptResult

    def principal : Principal = _currentPrincipal

    /**
      * Get the directory path part of a file path. Returns "." if the path is just
      * a single component.
      *
      * @param path a file path
      * @return the directory path
      */
    def dirname(path : String) : String = {
        val i = path.lastIndexOf('/')
        if (i < 0) "." else path.substring(0, i)
    }

    /**
      * Get the file extension of a given file path. If the path has no extension,
      * ".js" is returned.
      *
      * @param path the file path
      * @return the file extension
      */
    def extension(path : String) : String = {
        val i = path.lastIndexOf(".")
        if (i < 0) ".js" else path.substring(i)
    }

    def getNodeModulePaths(dirpath : String) : ScriptObjectMirror = {
        val result = engine.eval("[]").asInstanceOf[ScriptObjectMirror]
        val parts = dirpath split '/'
        if (parts.length == 0) result.setSlot(0, "/node_modules")
        else {
            for (i ← parts.indices) {
                val partsPath = s"""${parts.slice(0, parts.length - i).mkString("/")}/node_modules"""
                result.setSlot(i, partsPath)
            }
        }
        result
    }

    def getExtensionHandler(extension : String) : String ⇒ ScriptObjectMirror = {
//        if (extension == ".js") {
//
//        }
//        else if (extension == ".json") {
//
//        }
//        else null
        throw new ScriptException("not yet implemented")
    }

    /**
      * Get the module for the main script. This is usually the oldest module on moduleStack,
      * but when the module for the main script is being created, it will be the argument
      * module.
      *
      * @param m the current module (used only if moduleStack is empty)
      * @return the module for the main script
      */
    def getMain(m : ScriptObjectMirror) : ScriptObjectMirror = if (moduleStack.isEmpty) m else moduleStack.head

    /**
      * Run the main script.
      */
    override def run() : Unit = {
        // Read the main script and wrap it in a function.
        val content = try { Helpers.readWholeThing(script) } finally script close ()
        val loadScriptFn = wrapScript(content, scriptPath)

        // Create the module and the require function for the main script.
        val wrapArgs = newModule(scriptPath, None)
        val exports = wrapArgs.getSlot(0).asInstanceOf[ScriptObjectMirror]
        val requireFn = wrapArgs.getSlot(1).asInstanceOf[ScriptObjectMirror]
        val m = wrapArgs.getSlot(2).asInstanceOf[ScriptObjectMirror]
        moduleStack.push(m)

        try {
            // Create a binding for the global args[] array containing the main script arguments.
            val engineBindings = engine.getBindings(ScriptContext.ENGINE_SCOPE)
            args match {
                case Some(ja) ⇒
                    val json = compactRender(ja) //.replaceAllLiterally("\\\"", "\\\\\"")
                    engine.put("argsJson", json)
                    engine.eval(s"args = JSON.parse(argsJson);")
                    engineBindings.remove("argsJson")
                case None ⇒
                    engine.eval(s"args = [];")
            }
            // Call the function which wraps the main script.
            val result = loadScriptFn.call(exports, exports, requireFn, m, wrapArgs.getSlot(3), wrapArgs.getSlot(4))

            // Not sure this is needed yet. But it would be if scripts could launch asynchronous activities.
            m.setMember("loaded", true)

            // how to get the return value from main?
            // -- module.exports: consistent with other modules
            // -- return value of wrapper function: awkward because the script itself is not a function,
            //        so using "return" would bother IDE's, even though it would work within the wrapper
            // -- call a special external function (e.g. "exit") to return a value and terminate the
            //        script. The exit function probably would have to throw a caught exception. Maybe
            //        that exception could contain the return value. Not really all that different from
            //        when a script throws an uncaught exception (which are also caught outside the
            //        script. Do we care if scripts can catch the exit exception?
            _scriptResult = (1, Launcher.convertValue(result))
        }
        catch {
            case ex : Exception ⇒
                val msg = Option(ex.getMessage) getOrElse ex.toString
                _scriptResult = (-1, msg)
        }
        finally {
            // These internal modules have an entry point to ensure that any file handles
            // they opened are closed.
            if (internalModules contains "choiceletlib") jsChoiceletLib closeAll ()
            if (internalModules contains "filelib") jsFileLib closeAll ()
        }

    }

    /**
      * This produces a JavaScript function that can be called to load the contents of a script.
      * The function includes a number arguments, including a reference to an "exports" object,
      * which the script uses to export things to the parent module. The caller is responsible
      * for providing these arguments when calling the function:
      *
      *   exports {object} reference to module.exports object (also use for 'this')
      *   require {function} the module require function
      *   module {object} the module object for this script
      *   __filename {string} the script filename
      *   __dirname {string} the script directory path
      *
      * Upon return from the function, module.exports should reference an object containing
      * the items exported from the script. It may or may not be the object referenced by
      * module.exports on entry to the function.
      *
      * @param content the script text
      * @param scriptPath the script path
      * @return a callable ScriptObjectMirror
      */
    private def wrapScript(content : String, scriptPath : String) : ScriptObjectMirror = {
        // Maybe UTF-8 isn't supposed to have BOM, but maybe it does anyway
        if (content.charAt(0) == 0xFEFF) wrapScript(content.substring(1), scriptPath)
        else {
            val engineBindings = engine.getBindings(ScriptContext.ENGINE_SCOPE)
            val savePath = {
                val curPath = engineBindings.get(ScriptEngine.FILENAME)
                engineBindings.put(ScriptEngine.FILENAME, scriptPath)
                curPath
            }
            val fn = engine.eval(s"""(function (exports, require, module, __filename, __dirname) { $content })""")
                                    .asInstanceOf[ScriptObjectMirror]
            engineBindings.put(ScriptEngine.FILENAME, savePath)
            fn
        }
    }

    /**
      * Generate a new module object.
      *
      * @param filename the module filename
      * @param parent parent module if this is not the top-level script
      * @return
      */
    private def newModule(filename : String, parent : Option[ScriptObjectMirror]) : ScriptObjectMirror = {
        val m = moduleBuilder.call(this, this, filename, parent.orNull).asInstanceOf[ScriptObjectMirror]
        parent foreach { addChildModule(_, m) }
        m
    }

    private def addChildModule(parent : ScriptObjectMirror, m : ScriptObjectMirror) : ScriptObjectMirror = {
        val children = parent.getMember("children").asInstanceOf[ScriptObjectMirror]
        if (children.isArray) {
            children.setSlot(children.size(), m)
        }
        m
    }

    def require(name : String, parent : ScriptObjectMirror) : Any = {
        val module =
            if (isInternal(name)) {
                val internalName = if (name == "grouplib") "userlib" else name
                if (moduleCache hasMember internalName) {
                    // Get the module for the internal library
                    moduleCache.getMember(internalName).asInstanceOf[ScriptObjectMirror]
                }
                else if (!(internalModules contains internalName)) {
                    val lib = internalName match {
                        case "filelib" ⇒
                            jsFileLib = new JsFileLib(this, engine)
                            jsFileLib
                        case "userlib" ⇒
                            jsUserLib = new JsUserLib(this, engine)
                            jsUserLib
                        case "attrlib" ⇒
                            jsAttrLib = new JsAttrLib(this, engine)
                            jsAttrLib
                        case "httplib" ⇒
                            jsHttpLib = new JsHttpLib(this, engine)
                            jsHttpLib
                        case "choiceletlib" ⇒
                            jsChoiceletLib = new JsChoiceletLib(this, engine)
                            jsChoiceletLib
                        case _ ⇒ throw new ScriptException(s"logic error: $internalName")
                    }
                    val m = newModule(internalName, Some(parent))
                    m.setMember("exports", lib)
                    m
                }
                else {
                    throw new ScriptException(s"attempt to reload $internalName")
                }
            }
            else {
                // Not an internal module. Is it already cached?
                if (moduleCache hasMember name) {
                    (moduleCache getMember name).asInstanceOf[ScriptObjectMirror]
                }
                else {
                    val filename = resolve(name, parent)
                    val content = Cfs.withExistingFile(filename, principal, CfsOpenOptions.Default) {
                        case plain : CfsPlain ⇒
                            val rdr = CfsFiles.newBufferedReader(plain)
                            val text = try {
                                Helpers.readWholeThing(rdr)
                            } finally {
                                rdr close()
                            }
                            plain.setExpectedClosed()
                            Full(text)
                    } openOr {
                        throw new ScriptException(s"unable to read module $filename")
                    }
                    val loadScriptFn =
                        if (filename endsWith ".json") {
                            // If it's a JSON file, just export whatever the JSON encodes.
                            wrapScript(s"module.exports = $content;", filename)
                        }
                        else {
                            wrapScript(content, filename)
                        }
                    val wrapArgs = newModule(filename, Some(parent))
                    val exports = wrapArgs.getSlot(0).asInstanceOf[ScriptObjectMirror]
                    val requireFn = wrapArgs.getSlot(1).asInstanceOf[ScriptObjectMirror]
                    val m = wrapArgs.getSlot(2).asInstanceOf[ScriptObjectMirror]
                    moduleStack push m
                    // Unlike the main script, we don't care about the return value. Only what it exports.
                    loadScriptFn.call(exports, exports, requireFn, m, wrapArgs.getSlot(3), wrapArgs.getSlot(4))
                    m.setMember("loaded", true)
                    moduleStack.pop()
                    m
                }
            }
        addChildModule(parent, module)
        module.getMember("exports")
    }

    def resolve(name : String, module : ScriptObjectMirror, dirpath : String = "") : String = {
        if (dirpath.isEmpty && isInternal(name)) name
        else {
            val basename =
                if (name startsWith "/") name
                else {
                    val currentDir =
                        if (dirpath.isEmpty) dirname((module getMember "filename").asInstanceOf[String])
                        else dirpath
                    if (name startsWith "./") {
                        resolve(name.substring(2), module, currentDir)
                    }
                    else if (name startsWith "../") {
                        val parentDir = dirname(currentDir)
                        resolve(name.substring(3), module, if (parentDir == ".") "/" else parentDir)
                    }
                    else if (dirpath.isEmpty) name else s"$dirpath/$name"
                }
            if (basename startsWith "/") searchExtensions(basename)
            else searchModulePaths(basename, module)
        }
    }

    private def searchModulePaths(basename : String, module : ScriptObjectMirror) : String = {
        val paths = (module getMember "paths").asInstanceOf[ScriptObjectMirror]
        def helper(index : Int) : String = {
            if (index >= paths.size()) {
                throw new ScriptException(s"module $basename not found")
            }
            tryo(searchExtensions(s"${paths.getSlot(index).asInstanceOf[String]}/$basename")) match {
                case Full(name) ⇒ name
                case _ : EmptyBox ⇒ helper(index + 1)
            }
        }
        helper(0)
    }

    private def searchExtensions(basename : String) : String = {
        val nameBox =
            if (basename.lastIndexOf('.') < 0) {
                // File does not have an extension, so search
                Cfs.withExistingFile(basename, principal, CfsOpenOptions.Default) {
                    case _ : CfsPlain ⇒ Full(basename)
                } or Cfs.withExistingFile(s"$basename.js", principal, CfsOpenOptions.Default) {
                    case _ : CfsPlain ⇒ Full(s"$basename.js")
                } or Cfs.withExistingFile(s"$basename.json", principal, CfsOpenOptions.Default) {
                    case _ : CfsPlain ⇒ Full(s"$basename.json")
                }
            }
            else {
                // File already has a specified extension. See if it exists.
                Cfs.withExistingFile(basename, principal, CfsOpenOptions.Default) {
                    case _ : CfsPlain ⇒ Full(basename)
                }
            }
        nameBox openOr {
            throw new ScriptException(s"module $basename not found")
        }
    }

    private def isInternal(name : String) : Boolean = Set("filelib",
                                                          "userlib", "grouplib",
                                                          "attrlib", "httplib",
                                                          "choiceletlib") contains name

    def createJSObject : ScriptObjectMirror = objectConstructor.newObject().asInstanceOf[ScriptObjectMirror]

    /**
      * Set the current principal to a specified user or group. This is only permitted
      * when the original principal is a system administrator.
      *
      * @param path the path to the user or group
      * @return the new principal, boxed
      */
    def setPrincipal(path : String) : Box[Principal] = {
        if (originalPrincipal.isSystemAdmin_?) {
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
}

object JavaScriptContext {
    private val moduleCreationFunction =
        """
          |function(jsContext, id, parent) {
          |  function Module(id, parent) {
          |    if (parent) this.parent = parent;
          |    jsContext.moduleCache[id] = this;
          |    this.children = [];
          |    this.exports = {};
          |    this.filename = id;
          |    this.id = id;
          |    this.loaded = false;
          |    this.parent = parent;
          |    this.paths = jsContext.getNodeModulePaths(jsContext.dirname(id));
          |  }
          |  Module.prototype.require = function(path) {
          |    return jsContext.require(path, this);
          |  };
          |  Module.prototype.load = function(filename) {
          |    this.filename = filename;
          |    this.paths = jsContext.getNodeModulePaths(jsContext.dirname(filename));
          |    var handler = jsContext.getExtensionHandler(jsContext.extension(filename));
          |    handler(this, filename);
          |    this.loaded = true;
          |  };
          |  function makeRequire(m) {
          |    function require(path) {
          |      return m.require(path);
          |    }
          |    function resolve(request) {
          |      return jsContext.resolve(request, m);
          |    }
          |    require.resolve = resolve;
          |    require.main = jsContext.getMain(m);
          |    require.cache = jsContext.moduleCache;
          |    return require;
          |  }
          |  var m = new Module(id, parent);
          |  var require = makeRequire(m);
          |  return [m.exports, require, m, id, jsContext.dirname(id)];
          |}
        """.stripMargin
}

object Launcher {

    def run(script : CfsJsScript, principal : Principal,
            args : Option[JArray], options : JsRunOptions) : Box[(Int, Any)] = tryo {
        val rdr = CfsFiles.newBufferedReader(script)
        val scriptPath = script.getPath.toString
        val context = JavaScriptContext(options, principal, rdr, scriptPath, args, None, None, None)
        if (options.modules getOrElse false) {
            context.run()
            context.scriptResult
        }
        else {
            try {
//                val context = JavaScriptContext(principal)
                val engine = context.getEngine
//                val factory = new jdk.nashorn.api.scripting.NashornScriptEngineFactory()
//                val engine = factory.getScriptEngine("--no-java").asInstanceOf[NashornScriptEngine]
                val filelib = new JsFileLib(context, engine)
                val userlib = new JsUserLib(context, engine)
                val attrlib = new JsAttrLib(context, engine)
                val httplib = new JsHttpLib(context, engine)
                val choiceletlib = new JsChoiceletLib(context, engine)
                val engineBindings = engine.getBindings(ScriptContext.ENGINE_SCOPE)
                engineBindings.put("filelib", filelib)
                engineBindings.put("userlib", userlib)
                engineBindings.put("grouplib", userlib)
                engineBindings.put("attrlib", attrlib)
                engineBindings.put("httplib", httplib)
                engineBindings.put("choiceletlib", choiceletlib)
                engineBindings.put("SCRIPT_PATH", scriptPath)
                engineBindings.put("__name__", "__main__")
                engineBindings.put("__file__", scriptPath)
//                val rootFolder = new JsFolder(filelib, null)
//                val scriptFolder = (script.getPath.getParent.allParts foldLeft rootFolder) { (folder, child) ⇒
//                    folder getFolder child
//                }
//                JsModule.enable(engine, scriptFolder)
                val consoleLog = new StringWriter(1024)
                engine.getContext.setWriter(consoleLog)
                try {
                    args match {
                        case Some(ja) ⇒
                            val json = compactRender(ja) //.replaceAllLiterally("\\\"", "\\\\\"")
                            engine.put("argsJson", json)
                            engine.eval(s"args = JSON.parse(argsJson);")
                            engineBindings.remove("argsJson")
                        case None ⇒
                            engine.eval(s"args = [];")
                    }
                    engineBindings.put(ScriptEngine.FILENAME, scriptPath)
                    val result = engine.eval(rdr)
                    val log = consoleLog.toString
                    println(log)
                    (1, convertValue(result))
                }
                catch {
                    case ex : Exception ⇒
                        val msg = Option(ex.getMessage) getOrElse ex.toString
                        (-1, msg)
                }
                finally {
                    choiceletlib closeAll ()
                    filelib closeAll ()
                }
            }
            finally {
                // Closing rdr closes "script"
                rdr close ()
            }
        }
    }

    def convertValue(value : Any, refs : List[ScriptObjectMirror] = Nil) : Any = {
        value match {
            case u if ScriptObjectMirror.isUndefined(u) ⇒ null
            case array : Array[Any] ⇒ array map (elem ⇒ convertValue(elem, refs))
            case sm : ScriptObjectMirror ⇒
                if (refs contains sm) "[Object]"
                else if (sm.isFunction) "function"
                else if (sm.isArray) {
                    val len = sm.get("length").asInstanceOf[Int]
                    (0 until len).map(i ⇒ convertValue(sm.get(i.toString), sm :: refs)).toArray
                }
                else {
                    val pairs = sm.getOwnKeys(true) map { key ⇒
                        (key, convertValue(sm.get(key), sm :: refs))
                    }
                    Map(pairs : _*)
                }
            case d : Double ⇒
                if (d.isNaN) "NaN"
                else if (d.isPosInfinity) "+Infinity"
                else if (d.isNegInfinity) "-Infinity"
                else if (d.isWhole) d.toLong
                else d
            case other ⇒ other
        }
    }

    def stringField(obj : JSObject, member : String) : String = {
        if (obj.hasMember(member)) {
            obj.getMember(member) match {
                case s : String ⇒ s
                case _ ⇒ throw new ScriptException(s"field '$member' is not a string")
            }
        }
        else throw new ScriptException(s"missing required field '$member' of type String")
    }

    def optStringField(obj : JSObject, member : String) : Option[String] = {
        if (obj.hasMember(member)) {
            obj.getMember(member) match {
                case s : String ⇒ Some(s)
                case null ⇒ None
                case _ ⇒ throw new ScriptException(s"field '$member' is not a string")
            }
        }
        else None
    }

    def optLongField(obj : JSObject, member : String) : Option[Long] = {
        if (obj.hasMember(member)) {
            val v = obj.getMember(member) match {
                case n : java.lang.Integer ⇒ n.longValue()
                case n : java.lang.Long ⇒ n.longValue()
                case n : java.lang.Double ⇒ n.longValue()
                case n : java.lang.Number ⇒ n.longValue()
                case other ⇒ other.asInstanceOf[Long]
            }
            Some(v)
        }
        else None
    }

    def booleanField(obj : JSObject, member : String) : Boolean = {
        if (obj.hasMember(member)) {
            val v = obj.getMember(member)
            v match {
                case b : java.lang.Boolean ⇒ b.booleanValue()
                case _ ⇒ throw new ScriptException(s"field '$member' is not a Boolean")
            }
        }
        else throw new ScriptException(s"missing required field '$member' of type Boolean")
    }

    def optBooleanField(obj : JSObject, member : String) : Option[Boolean] = {
        if (obj.hasMember(member)) {
            obj.getMember(member) match {
                case b : java.lang.Boolean ⇒ Some(b.booleanValue())
                case null ⇒ None
                case _ ⇒ throw new ScriptException(s"field '$member' is not a Boolean")
            }
        }
        else None
    }

    def errorFailure(f : Failure)(implicit engine : ScriptEngine) : JSObject = {
        val obj = makeObject()
        obj.setMember("status", -1)
        obj.setMember("msg", f.messageChain)
        obj
    }

    def emptyFailure()(implicit engine : ScriptEngine) : JSObject = {
        val obj = makeObject()
        obj.setMember("status", -1)
        obj.setMember("msg", "Empty")
        obj
    }

    def boxedMapToObject(mbox : Box[Map[String, Any]])(implicit engine : ScriptEngine) : JSObject = {
        mbox match {
            case Full(m) ⇒ mapToObject(m)
            case Empty ⇒ emptyFailure()
            case f : Failure ⇒ errorFailure(f)
        }
    }

    /**
      * Convert a value returned by a library method, implemented in Scala, to
      * a value which will behave as, if not appear as, a native JavaScript
      * value or object, when returned to a JavaScript script.
      *
      * @param v the value to be converted
      * @param engine the JavaScript engine
      * @return the JavaScript-compatible equivalent value
      */
    def valueForJS(v : Any)(implicit engine : ScriptEngine) : Any = {
        v match {
            case null ⇒ null
            case s : String ⇒ s
            case n : Long ⇒ n.toDouble
            case b : Boolean ⇒ b
            case d : Double ⇒ d
            case i : Int ⇒ i.toDouble
            case b : Byte ⇒ b.toDouble
            case s : Short ⇒ s.toDouble
            case f : Float ⇒ f.toDouble
            case Some(x) ⇒ valueForJS(x)(engine)
            case None ⇒ null
            case a : Array[_] ⇒ arrayForJS(a)(engine)
            case list : List[_] ⇒ listForJS(list)(engine)
            case m : Map[_, _] ⇒ mapForJS(m)(engine)
            case (e1, e2) ⇒
                val result = makeArray()(engine)
                result.setSlot(0, valueForJS(e1))
                result.setSlot(1, valueForJS(e2))
                result
            case (e1, e2, e3) ⇒
                val result = makeArray()(engine)
                result.setSlot(0, valueForJS(e1))
                result.setSlot(1, valueForJS(e2))
                result.setSlot(2, valueForJS(e3))
                result
            case other ⇒
                throw new ScriptException(s"cannot return ${other.toString} to JavaScript")
        }
    }

    /**
      * Convert a Scala map to an object suitable for returning to a JavaScript script.
      *
      * @param m the map
      * @param engine the script engine
      * @return a JSObject representing the map
      */
    def mapForJS(m : Map[_, _])(implicit engine : ScriptEngine) : JSObject = {
        val result = makeObject()(engine)
        m.foldLeft(result) {
            case (jsobj, (key : String, elem)) ⇒
                jsobj.setMember(key, valueForJS(elem))
                jsobj
            case (jsobj, _) ⇒ jsobj      // Ignore any non-String keys
        }
    }

    def arrayForJS(a : Array[_])(implicit engine : ScriptEngine) : JSObject = {
        val result = makeArray()(engine)
        a.zipWithIndex.foldLeft(result) { case (jsarray, (elem, i)) ⇒
            jsarray.setSlot(i, valueForJS(elem)(engine))
            jsarray
        }
    }

    def listForJS(list : List[_])(implicit engine : ScriptEngine) : JSObject = {
        val result = makeArray()(engine)
        list.zipWithIndex.foldLeft(result) { case (jsarray, (elem, i)) ⇒
            jsarray.setSlot(i, valueForJS(elem)(engine))
            jsarray
        }
    }

    def mapToObject(m : Map[String, Any])(implicit engine : ScriptEngine) : JSObject = mapForJS(m)(engine)

    /**
      * Create a JavaScript Array object, which typically will be populated by a library
      * method (implemented in Scala) and returned to the JavaScript which called it.
      *
      * @param engine the JavaScript engine
      * @return an object supporting the JSObject interface, and behaving as an array
      */
    def makeArray()(implicit engine : ScriptEngine) : JSObject = {
        engine.get("Array").asInstanceOf[JSObject].newObject().asInstanceOf[JSObject]
    }

    /**
      * Create a JavaScript object, which typically will be populated by a library method
      * (implemented in Scala) and returned to the JavaScript script which called it.
      *
      * @param engine the JavaScript engine
      * @return an object supporting the JSObject interface
      */
    def makeObject()(implicit engine : ScriptEngine) : JSObject = {
        val objCtr = engine.get("Object").asInstanceOf[JSObject]
        objCtr.newObject().asInstanceOf[JSObject]
    }
}
