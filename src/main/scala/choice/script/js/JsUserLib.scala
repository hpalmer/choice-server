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
  * @author Howard Palmer
  * Created by Hep on 10/21/2016.
  */
package choice.script.js

import javax.script.{ScriptEngine, ScriptException}

import choice.access.Principal
import choice.fs._
import choice.script.UserDefinition
import jdk.nashorn.api.scripting.{JSObject, ScriptObjectMirror}
import jdk.nashorn.internal.runtime.JSType
import net.liftweb.common._

import scala.tools.jline_embedded.internal.Nullable

class JsUserLib(private val context : JavaScriptContext, private implicit val engine : ScriptEngine) {

    private def cfsUserLib = context.cfsUserLib

    def principal : Principal = context.principal

    def mkgroup(path : String, @Nullable gdesc : JSObject) : JSObject = {
        val groupDesc = Option(gdesc) map { gdesc ⇒
            val desc = Launcher.optStringField(gdesc, "description")
            val login = Launcher.optBooleanField(gdesc, "login") orElse Some(false)
            val googleLoginEnabled = Launcher.optBooleanField(gdesc, "googleLoginEnabled") orElse Some(false)
            val signup = Launcher.optBooleanField(gdesc, "signup") orElse Some(false)
            val googleSignupEnabled = Launcher.optBooleanField(gdesc, "googleSignupEnabled") orElse Some(false)
            val captcha = Launcher.optBooleanField(gdesc, "captcha") orElse Some(false)
            val verifyEmailEnabled = Launcher.optBooleanField(gdesc, "verifyEmailEnabled") orElse Some(false)
            val regKey = Launcher.optStringField(gdesc, "regKey")
            val guest = Launcher.optStringField(gdesc, "guestPage")
            val home = Launcher.optStringField(gdesc, "homePage")
            GroupDesc(None, desc, login, googleLoginEnabled, signup, googleSignupEnabled, captcha,
                verifyEmailEnabled, regKey, guest, home)
        }
        val result = cfsUserLib.mkgroup(path, groupDesc) map (_ + (("status", 1)))
        Launcher.boxedMapToObject(result)
    }

    def mkuser(gpath : String, username : String, @Nullable password : String,
               @Nullable email : String, @Nullable regcode : String) : JSObject = {
        val result = cfsUserLib.mkuser(gpath, username, Option(password), Option(email), Option(regcode))
        Launcher.boxedMapToObject(result)
    }

    def mkuser_no_password(gpath : String, username : String,
                           @Nullable email : String, @Nullable regcode : String) : JSObject = {
        val result = cfsUserLib.mkuser(gpath, username, None, Option(email), Option(regcode), oauthonly = true)
        Launcher.boxedMapToObject(result)
    }

    def mkusers(gpath : String, users : Array[JSObject]) : JSObject = {
        val udefs = users map { jsobj ⇒
            try {
                val username = JSType.toString(jsobj.getMember("username"))
                val password =
                    if (jsobj.hasMember("password")) Some(JSType.toString(jsobj.getMember("password")))
                    else None
                val email =
                    if (jsobj.hasMember("email")) Some(JSType.toString(jsobj.getMember("email")))
                    else None
                val regcode =
                    if (jsobj.hasMember("regcode")) Some(JSType.toString(jsobj.getMember("regcode")))
                    else None
                val oauthonly =
                    if (jsobj.hasMember("oauthonly")) JSType.toBoolean(jsobj.getMember("oauthonly"))
                    else false
                UserDefinition(username, password, email, regcode, oauthonly)
            }
            catch {
                case _ : Exception ⇒ null
            }
        }
        cfsUserLib.mkusers(gpath, udefs) match {
            case Full(mlist) ⇒ Launcher.mapToObject(Map("status" → 1, "users" → mlist))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def findUser(group : String, username : String, recursive : Boolean) : JSObject = {
        cfsUserLib.findUser(group, username, recursive) match {
            case Full(uinfo) ⇒
                val result = uinfo.asMap + ("status" → 1)
                uinfo close ()
                Launcher.mapToObject(result)
            case Empty ⇒ null
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def listUsers(gpath : String, @Nullable recursive : Boolean) : JSObject = {
        val recursiveFlag = Option(recursive) getOrElse false
        cfsUserLib.listUsers(gpath, recursiveFlag) match {
            case Full(ulist) ⇒
                val maplist = ulist map { uinfo ⇒
                    val result = uinfo.asMap
                    uinfo close ()
                    result
                }
                Launcher.mapToObject(Map("status" → 1, "count" → maplist.length, "users" → maplist))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def login(username : String, password : String, @Nullable group : Any) : JSObject = {
        val (groupName, groupId) = decodeGroup(group)
        cfsUserLib.login(username, password, groupName, groupId) match {
            case Full(_) ⇒ getSession
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def logout() : Boolean = {
        val result = cfsUserLib.logout()
        result openOr false
    }

    def getSession : JSObject = {
        val result = cfsUserLib.getSession map (_ + (("status", 1)))
        Launcher.boxedMapToObject(result)
    }

    def getLoginGroup : String = {
        val result = cfsUserLib.getLoginGroup
        result match {
            case Full(s) ⇒ s
            case Empty ⇒ throw new ScriptException("result was Empty")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    def isLoggedIn : Boolean = {
        val result = cfsUserLib.isLoggedIn
        result match {
            case Full(b) ⇒ b
            case Empty ⇒ throw new ScriptException("result was Empty")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    def checkUser(username : String, password : String, @Nullable group : Any) : JSObject = {
        val (groupName, groupId) = decodeGroup(group)
        cfsUserLib.checkUser(username, password, groupName, groupId) match {
            case Full((status, msg)) ⇒
                Launcher.mapToObject(Map("status" → status, "msg" → msg))
            case Empty ⇒ Launcher.emptyFailure()
            case f : Failure ⇒ Launcher.errorFailure(f)
        }
    }

    def canRegister(@Nullable group : Any) : JSObject = {
        val (groupName, groupId) = decodeGroup(group)
        val result = cfsUserLib.canRegister(groupName, groupId)
        Launcher.boxedMapToObject(result)
    }

    def resetPassword(username : String, newPassword : String, @Nullable group : Any) : Boolean = {
        val (groupName, _) = decodeGroup(group)
        cfsUserLib.resetPassword(username, newPassword, groupName) match {
            case Full(b) ⇒ b
            case Empty ⇒ throw new ScriptException("result was Empty")
            case f : Failure ⇒ throw new ScriptException(f.messageChain)
        }
    }

    private def decodeGroup(@Nullable group : Any) : (Option[String], Option[Long]) = {
        Option(group) match {
            case None ⇒ (None, None)
            case Some(obj : ScriptObjectMirror) ⇒ obj.getDefaultValue(null) match {
                case s : String ⇒ (Some(s), None)
                case n : Number ⇒ (None, Some(n.longValue()))
                case _ ⇒ (None, None)
            }
            case Some(s : String) ⇒ (Some(s), None)
            case Some(n : Number) ⇒ (None, Some(n.longValue()))
            case Some(_) ⇒
                throw new ScriptException("decodeGroup argument error")
        }
    }
}
