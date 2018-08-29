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
  * Cfs file operations for JavaScript.
  *
  * @author Howard Palmer
  * Created by Hep on 10/6/2016.
  */
package choice.script.js

import java.io.PrintWriter
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{OpenOption, StandardOpenOption}
import javax.script.{ScriptEngine, ScriptException}

import choice.access.Principal
import choice.fs._
import jdk.nashorn.api.scripting.{JSObject, ScriptObjectMirror}
import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._

import scala.tools.jline_embedded.internal.Nullable


class LineReader(private val cfsPlain : CfsPlain, private implicit val engine : ScriptEngine) {
    private var _in : Option[CfsBufferedReader] = None

    private def getReader : CfsBufferedReader = {
        _in match {
            case Some(in) ⇒ in
            case None ⇒
                tryo(CfsFiles.newBufferedReader(cfsPlain)) match {
                    case Full(rdr) ⇒
                        _in = Some(rdr)
                        rdr
                    case Empty ⇒ throw new ScriptException("getInputStream returned Empty")
                    case f : Failure ⇒ throw new ScriptException(f.messageChain)
                }
        }
    }

    def readLine() : String = {
        getReader.readLine()
    }

    def readLines() : JSObject = {
        val lines = Launcher.makeArray()
        def helper(slot : Int) : JSObject = {
            val s = readLine()
            if (s == null) lines
            else {
                lines setSlot (slot, s)
                helper(slot + 1)
            }
        }
        helper(0)
    }

    def reset() : Unit = {
        _in foreach (_.close())
        _in = None
    }
}

class JsFileLib(private val context : JavaScriptContext, private implicit val engine : ScriptEngine) {

    private var _openedReaders = List.empty[CfsBufferedReader]
    private var _openedWriters = List.empty[CfsBufferedWriter]

    private def cfsFileLib = context.cfsFileLib

    private def principal : Principal = context.principal

    def isValidPath(path : String) : Boolean = cfsFileLib.isValidPath(path)

    def isValidFilename(name : String) : Boolean = cfsFileLib.isValidFilename(name)

    def isAbsolutePath(path : String) : Boolean = cfsFileLib.isAbsolutePath(path)

    def isRelativePath(path : String) : Boolean = cfsFileLib.isRelativePath(path)

    def lookup(path : String) : JSObject = {
        val bmap = cfsFileLib.lookup(path)
        Launcher.boxedMapToObject(bmap)
    }

    def lookupById(id : Long) : JSObject = {
        val result = cfsFileLib.lookupById(id)
        Launcher.boxedMapToObject(result)
    }

    def openOption(name : String) : OpenOption = {
        name.toLowerCase match {
            case "read" ⇒ StandardOpenOption.READ
            case "write" ⇒ StandardOpenOption.WRITE
            case "append" ⇒ StandardOpenOption.APPEND
            case "create" ⇒ StandardOpenOption.CREATE
            case "create_new" ⇒ StandardOpenOption.CREATE_NEW
            case "truncate_existing" ⇒ StandardOpenOption.TRUNCATE_EXISTING
            case _ ⇒ throw new ScriptException(s"unknown OpenOption $name")
        }
    }

    def charSet(name : String) : Charset = {
        name.toLowerCase.replaceAllLiterally("-", "_") match {
            case "utf_8" | "utf8" ⇒ StandardCharsets.UTF_8
            case "utf_16" | "utf16" ⇒ StandardCharsets.UTF_16
            case "iso_8859_1" | "iso8859_1" ⇒ StandardCharsets.ISO_8859_1
            case "us_ascii" | "usascii" | "ascii" ⇒ StandardCharsets.US_ASCII
            case "utf_16be" | "utf16be" ⇒ StandardCharsets.UTF_16BE
            case "utf_16le" | "utf16le" ⇒ StandardCharsets.UTF_16LE
            case _ ⇒ throw new ScriptException(s"unknown Charset $name")
        }
    }

    def newBufferedReader(path : String) : CfsBufferedReader = {
        newBufferedReader(path, StandardCharsets.UTF_8)
    }

    def newBufferedReader(path : String, cs : Charset) : CfsBufferedReader = {
        val rbox = cfsFileLib.getAbsolutePath(path) flatMap { cfspath ⇒
            tryo(CfsFiles.newBufferedReader(cfspath, cs)(principal _))
        }
        rbox match {
            case Full(rdr) ⇒
                _openedReaders = rdr :: _openedReaders
                rdr.onClose({ r ⇒
                    _openedReaders = _openedReaders filterNot (_ eq r)
                })
                rdr
            case Empty ⇒ throw new ScriptException("Empty result")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    def newBufferedWriter(path : String) : CfsBufferedWriter = {
        newBufferedWriter(path, StandardCharsets.UTF_8, new Array[OpenOption](0))
    }

    def newBufferedWriter(path : String, options : Array[OpenOption]) : CfsBufferedWriter = {
        newBufferedWriter(path, StandardCharsets.UTF_8, options)
    }

    def newBufferedWriter(path : String, cs : Charset, options : Array[OpenOption]) : CfsBufferedWriter = {
        val wbox = cfsFileLib.getAbsolutePath(path) flatMap { cfspath ⇒
            tryo(CfsFiles.newBufferedWriter(cfspath, cs, options : _*)(principal _))
        }
        wbox match {
            case Full(wtr) ⇒
                _openedWriters = wtr :: _openedWriters
                wtr.onClose({ w ⇒
                    _openedWriters = _openedWriters filterNot (_ eq w)
                })
                wtr
            case Empty ⇒ throw new ScriptException("Empty result")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    def newLineReader(path : String) : LineReader = {
        val pbox = cfsFileLib.getAbsolutePath(path) flatMap { cfspath ⇒
            Cfs open (cfspath, principal, CfsOpenOptions.Default) match {
                case Full(plain : CfsPlain) ⇒ Full(new LineReader(plain, engine))
                case Full(other) ⇒
                    other close ()
                    Failure(s"${cfspath.toString} is not a plain file")
                case Empty ⇒ Failure(s"${cfspath.toString} does not exist")
                case f : Failure ⇒ Failure(f.messageChain)
            }
        }
        pbox match {
            case Full(rdr) ⇒ rdr
            case Empty ⇒ throw new ScriptException("Empty result")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    def newPrintWriter(path : String) : PrintWriter = {
        newPrintWriter(path, StandardCharsets.UTF_8, new Array[OpenOption](0))
    }

    def newPrintWriter(path : String, options : Array[OpenOption]) : PrintWriter = {
        newPrintWriter(path, StandardCharsets.UTF_8, options)
    }

    def newPrintWriter(path : String, cs : Charset, options : Array[OpenOption]) : PrintWriter = {
        new PrintWriter(newBufferedWriter(path, cs, options))
    }

    def list(path : String) : JSObject = list(path, null)

    def list(path : String, @Nullable mimetype : String) : JSObject = {
        val mimeTypeOpt = Option(mimetype)
        val maps = cfsFileLib.list(path, mimeTypeOpt)
        maps match {
            case Full(list) ⇒
                Launcher.mapToObject(Map("status" → 1, "files" → list))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def mkdir(path : String, recursive : Boolean) : JSObject = {
        val result = cfsFileLib.mkdir(path, recursive)
        Launcher.boxedMapToObject(result)
    }

    def rm(path : String, recursive : Boolean) : JSObject = {
        val result = cfsFileLib.rm(path, recursive)
        Launcher.boxedMapToObject(result map (b ⇒ Map("status" → 1, "result" → b)))
    }

    def create(path : String, mimetype : String) : JSObject = {
        val result = cfsFileLib.create(path, mimetype) map (_ + (("status", 1)))
        Launcher.boxedMapToObject(result)
    }

    def chown(path : String, @Nullable owner : String) : JSObject = {
        val result = cfsFileLib.chown(path, Option(owner))
        Launcher.boxedMapToObject(result map (b ⇒ Map("status" → 1, "result" → b)))
    }

    def setMimeType(path : String, mimetype : String) : Boolean = {
        cfsFileLib.setMimeType(path, mimetype) match {
            case Full(b) ⇒ b
            case _ : EmptyBox ⇒
                throw new ScriptException(s"failed to set MIME type of $path")
        }
    }

    def share(policy : String, filespec : String) : JSObject = {
        val result = cfsFileLib.share(policy, filespec)
        Launcher.boxedMapToObject(result)
    }

    def cd(path : String) : JSObject = {
        cfsFileLib.cd(path) match {
            case Full(abspath) ⇒
                Launcher.mapToObject(Map("status" → 1, "cwd" → abspath.toString))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def pushd(path : String) : JSObject = {
        cfsFileLib.pushd(path) match {
            case Full(abspath) ⇒
                Launcher.mapToObject(Map("status" → 1, "cwd" → abspath.toString))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def popd() : String = {
        cfsFileLib.popd().toString
    }

    def writeJson(path : String, obj : JSObject) : JSObject = {
        val JSON = engine.eval("JSON").asInstanceOf[ScriptObjectMirror]
        val s = JSON.callMember("stringify", obj)
        val newdata = s.asInstanceOf[String]
        val fmap = cfsFileLib.writeJSON(path, newdata)
        fmap match {
            case Full(m) ⇒ Launcher.mapToObject(m + ("status" → 1))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def readJson(path : String) : JSObject = {
        val jval = cfsFileLib.readJSON(path) flatMap { json ⇒
            val JSON = engine.eval("JSON").asInstanceOf[ScriptObjectMirror]
            tryo(JSON.callMember("parse", json).asInstanceOf[JSObject])
        }
        jval match {
            case Full(jsobj) ⇒ jsobj
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def sendMail(message : JSObject, @Nullable mailer : String) : JSObject = {
        def listHelper(obj : JSObject, key : String) : List[String] = {
            if (obj hasMember key) {
                obj getMember key match {
                    case s : String ⇒ s :: Nil
                    case mo : ScriptObjectMirror ⇒
                        if (mo.isArray) {
                            (Range(0, mo.size()) map (i ⇒ mo getSlot i) collect {
                                case s : String ⇒ s
                            }).toList
                        }
                        else Nil
                }
            }
            else Nil
        }
        val mailerOpt = Option(mailer)
        val from = Launcher.optStringField(message, "from")
        val subject = Launcher.optStringField(message, "subject")
        val html = Launcher.optBooleanField(message, "html") getOrElse false
        val content = Launcher.optStringField(message, "content") getOrElse ""
        val headers = {
            if (message hasMember "headers") {
                message getMember "headers" match {
                    case mo : ScriptObjectMirror ⇒
                        val hdrs = mo.getOwnKeys(false) map { key ⇒
                            mo getMember key match {
                                case s : String ⇒ (key, s)
                                case o ⇒ (key, o.toString)
                            }
                        }
                        Some(Map(hdrs.toList : _*))
                }
            }
            else None
        }
        val msg = MailerMessage(from, listHelper(message, "to"),
                                listHelper(message, "cc"), listHelper(message, "bcc"),
                                subject, headers, html, content)
        cfsFileLib.sendMail(msg, mailerOpt) match {
            case Full(b) ⇒
                Launcher.mapToObject(Map("status" → 1, "result" → b))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def compileComponent(source : String, @Nullable output : String) : JSObject = {
        val result = cfsFileLib.compileComponent(source, Option(output))
        Launcher.boxedMapToObject(result)
    }

    def queueScript(queue : String, filename : String, scriptPath : String, args : String,
                    replace : Boolean = true, asOwner : Boolean = false) : Boolean = {
        cfsFileLib.queueScript(queue, filename, scriptPath, args, replace, asOwner) match {
            case Full(replaced) ⇒ replaced
            case Empty ⇒ throw new ScriptException(s"error queuing $filename: Empty result")
            case f : Failure ⇒ throw new ScriptException(s"error queuing $filename: ${f.messageChain}")
        }
    }

    def getQueuedScript(queue : String, filename : String) : JSObject = {
        cfsFileLib.getQueuedScript(queue, filename) match {
            case Full(qelem) ⇒
                Launcher.mapToObject(Map(
                    "status" → 1,
                    "queue" → queue,
                    "script" → qelem.script,
                    "principal" → qelem.principal,
                    "principalId" → qelem.principalId,
                    "asOwner" → qelem.asOwner,
                    "args" → qelem.args
                ))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def runQueuedScript(queue : String, filename : String, event : String, eventArgs : Array[Any]) : JSObject = {
        val jsargs : Array[JValue] = eventArgs map {
            case s : String ⇒ JString(s)
            case n : Int ⇒ JInt(BigInt(n))
            case d : Double ⇒ JDouble(d)
            case b : Boolean ⇒ JBool(b)
        }
        cfsFileLib.runQueuedScript(queue, filename, event, jsargs) match {
            case Full((status, result)) ⇒
                Launcher.mapToObject(Map("status" → status, "result" → result))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def readSession(path : String) : JSObject = {
        val smap = cfsFileLib.readSession(path)
        smap match {
            case Full(m) ⇒ Launcher.mapToObject(m)
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def setPrincipal(path : String) : Boolean = {
        context.setPrincipal(path) match {
            case Full(_) ⇒ true
            case _ ⇒ false
        }
    }

    def restorePrincipal() : JSObject = {
        Launcher.mapToObject(context.restorePrincipal().asMap)
    }

    def closeAll() : Unit = {
        // Copy the list head because the close actions will modify it
        val openReaders = _openedReaders
        openReaders foreach (_.close())
        val openWriters = _openedWriters
        openWriters foreach (_.close())
    }
}
