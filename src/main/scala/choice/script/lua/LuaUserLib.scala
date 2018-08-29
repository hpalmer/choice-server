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
  * Lua support for user and group operations.
  *
  * @author Howard Palmer
  * Created by Hep on 9/11/2014.
  */
package choice.script.lua

import choice.access.Principal
import choice.fs._
import choice.lib.GroupOps.DistElem
import choice.lib._
import choice.model.GlobalConfig
import choice.script.lua.LuaLauncher._
import net.liftweb.common._
import org.luaj.vm2._
import org.luaj.vm2.lib.{TwoArgFunction, VarArgFunction}

object UserLibV extends Enumeration {
    val mkgroup : Value = Value(0)
    val mkuser : Value = Value
    val mkuser_no_password : Value = Value
    val login : Value = Value
    val logout : Value = Value
    val getsession : Value = Value
    val getlogingroup : Value = Value
    val isloggedin : Value = Value
    val checkuser : Value = Value
    val canregister : Value = Value
    val assignsubgroup : Value = Value

    val names : Array[String] = this.values.toArray.map(_.toString)
}

class LuaUserLib(chenv : ChoiceEnviron) extends TwoArgFunction {

    override def call(modname : LuaValue, env : LuaValue) : LuaValue = {
        val t = new LuaTable()
        chenv.bind(t, classOf[UserLibV], UserLibV.names)
        env.set("userlib", t)
        env.get("package").get("loaded").set("userlib", t)
        t
    }
}

class UserLibV(chenv : ChoiceEnviron, nameS : String, opcodeS : Int) extends VarArgFunction {

    name = nameS
    opcode = opcodeS

    def principal : Principal = chenv.principal

    def getGroupAndGid(v : LuaValue) : (Option[String], Option[Long]) = {
        v match {
            case n : LuaInteger ⇒ (None, Some(n.tolong()))
            case s : LuaString ⇒ (Some(s.tojstring()), None)
            case _ ⇒ (None, None)
        }
    }

    override def invoke(args : Varargs) : Varargs = {
        UserLibV(opcode) match {
            case UserLibV.mkgroup ⇒
                val path = args.checkjstring(1)
                val gdesc = Option(args.opttable(2, null)) map { t ⇒
                    val desc = Option(t.get("description").optjstring(null))
                    val login = Option(t.get("loginEnabled").optboolean(false))
                    val googleLoginEnabled = Option(t.get("googleLoginEnabled").optboolean(false))
                    val signup = Option(t.get("signupEnabled").optboolean(false))
                    val googleSignupEnabled = Option(t.get("googleSignupEnabled").optboolean(false))
                    val captcha = Option(t.get("captchaEnabled").optboolean(false))
                    val verifyEmailEnabled = Option(t.get("verifyEmailEnabled").optboolean(false))
                    val regKey = Option(t.get("regKey").optjstring(null))
                    val guest = Option(t.get("guestPage").optjstring(null))
                    val home = Option(t.get("homePage").optjstring(null))
                    GroupDesc(None, desc, login, googleLoginEnabled, signup, googleSignupEnabled, captcha,
                        verifyEmailEnabled, regKey, guest, home)
                }
                _mkgroup(path, gdesc)
            case UserLibV.mkuser ⇒
                val gpath = args.checkjstring(1)
                val username = args.checkjstring(2)
                val password = Option(args.optjstring(3, null))
                val email = Option(args.optjstring(4, null))
                val regcode = args.optvalue(5, LuaValue.NIL) match {
                    case s : LuaString ⇒ Some(s.toString)
                    case i : LuaInteger ⇒ Some(i.toString)
                    case _ : LuaNil ⇒ None
                }
                _mkuser(gpath, username, password, email, regcode)
            case UserLibV.mkuser_no_password ⇒
                val gpath = args.checkjstring(1)
                val username = args.checkjstring(2)
                val email = Option(args.optjstring(3, null))
                val regcode = args.optvalue(4, LuaValue.NIL) match {
                    case s : LuaString ⇒ Some(s.toString)
                    case i : LuaInteger ⇒ Some(i.toString)
                    case _ : LuaNil ⇒ None
                }
                _mkuser_no_password(gpath, username, email, regcode)
            case UserLibV.login ⇒
                val username = args.checkjstring(1)
                val password = args.optjstring(2, "")
                val (group, gid) = getGroupAndGid(args.arg(3))
                _login(username, password, group, gid)
            case UserLibV.logout ⇒ _logout
            case UserLibV.getsession ⇒ _getsession
            case UserLibV.getlogingroup ⇒ _getlogingroup
            case UserLibV.isloggedin ⇒ _isloggedin
            case UserLibV.checkuser ⇒
                val username = args.checkjstring(1)
                val password = args.optjstring(2, "")
                val (group, gid) = getGroupAndGid(args.arg(3))
                _checkuser(username, password, group, gid)
            case UserLibV.canregister ⇒
                val (group, gid) = getGroupAndGid(args.arg(1))
                _canregister(group, gid)
            case UserLibV.assignsubgroup ⇒
                // First argument can be a group or a user path. If it's a group, the username follows
                val boxpair = {
                    val path = args.checkjstring(1)
                    Cfs.withExistingFile(path, principal, CfsOpenOptions.Default) {
                        case ginfo : GroupInfo ⇒
                            val username = args.checkjstring(2)
                            Full((ginfo.getPath / username.toLowerCase, 3))
                        case uinfo : UserInfo ⇒
                            Full((uinfo.getPath, 1))
                    }
                }
                val result = boxpair map {
                    case (userpath, iarg) ⇒
                        val list = {
                            args.arg(iarg) match {
                                case t : LuaTable ⇒
                                    val delist = luaTableToList(t) flatMap {
                                        case vt : LuaTable ⇒
                                            val group = Option(vt.get("group").optjstring(null))
                                            val weight = vt.get("weight") match {
                                                case n : LuaInteger ⇒ Some(n.toint())
                                                case _ ⇒ None
                                            }
                                            (group, weight) match {
                                                case (Some(g), Some(w)) ⇒ DistElem(g, w) :: Nil
                                                case _ ⇒ Nil
                                            }
                                        case _ ⇒ Nil
                                    }
                                    Some(delist)
                                case _ ⇒ None
                            }
                        }
                        _assignsubgroup(userpath, list)
                }
                result match {
                    case Full(va) ⇒ va
                    case Empty ⇒ errorResult("Empty result")
                    case f : Failure ⇒ errorResult(f.messageChain)
                }
        }
    }

    private def _mkgroup(path : String, gdesc : Option[GroupDesc]) : Varargs = {
        val result = chenv.cfsUserLib.mkgroup(path, gdesc)
        boxedMapToLuaTable(result)
    }

    private def _mkuser(gpath : String, username : String, password : Option[String],
                       email : Option[String], regcode : Option[String]) : Varargs = {
        val result = chenv.cfsUserLib.mkuser(gpath, username, password, email, regcode)
        boxedMapToLuaTable(result)
    }

    private def _mkuser_no_password(gpath : String, username : String,
                                    email : Option[String], regcode : Option[String]) : Varargs = {
        val result = chenv.cfsUserLib.mkuser(gpath, username, None, email, regcode, oauthonly = true)
        boxedMapToLuaTable(result)
    }

    private def _login(username : String, password : String,
                       group : Option[String], gid : Option[Long]) : Varargs = {
        val result = chenv.cfsUserLib.login(username, password, group, gid)
        val bmap : Box[Map[String, Any]] = result map { nclient ⇒
            val roles =
                if (GlobalConfig.isSystemAdmin_?(nclient.getPrincipal)) Full("ADMIN") else Empty
            val page = UserOps.getHomePage(nclient)
            Map("status" → 1, "role" → roles.toList, "redirect" → page)
        }
        boxedMapToLuaTable(bmap)
    }

    private def _logout : Varargs = {
        val result = chenv.cfsUserLib.logout()
        LuaValue.valueOf(result openOr false)
    }

    private def _getsession : Varargs = {
        val result = chenv.cfsUserLib.getSession
        boxedMapToLuaTable(result)
    }

    private def _getlogingroup : Varargs = {
        val result = chenv.cfsUserLib.getLoginGroup
        result match {
            case Full(lgpath) ⇒ LuaString.valueOf(lgpath)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    private def _isloggedin : Varargs = {
        val result = chenv.cfsUserLib.isLoggedIn
        result match {
            case Full(b) ⇒ LuaValue.valueOf(b)
            case Empty ⇒ errorResult("Empty result")
            case f : Failure ⇒ errorResult(f.messageChain)
        }
    }

    private def _checkuser(username : String, password : String,
                           group : Option[String], gid : Option[Long]) : Varargs = {
        import LuaValue._
        val result = chenv.cfsUserLib.checkUser(username, password, group, gid)
        result match {
            case Full((code, msg)) ⇒ varargsOf(valueOf(code), valueOf(msg))
            case Empty ⇒ varargsOf(valueOf(-1), valueOf("Empty result"))
            case f : Failure ⇒ varargsOf(valueOf(-1), valueOf(f.messageChain))
        }
    }

    private def _canregister(group : Option[String], gid : Option[Long]) : Varargs = {
        val result = chenv.cfsUserLib.canRegister(group, gid)
        boxedMapToLuaTable(result)
    }

    private def _assignsubgroup(upath : CfsAbsolutePath, dist : Option[List[DistElem]]) : Varargs = {
        val result = Cfs.withExistingFile(upath, principal) {
            case uinfo : UserInfo ⇒
                GroupOps.assignSubgroup(uinfo, dist)
        }
        boxedMapToLuaTable(result)
    }
}
