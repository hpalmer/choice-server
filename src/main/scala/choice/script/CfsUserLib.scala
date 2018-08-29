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
  * Created by Hep on 10/24/2016.
  */
package choice.script

import choice.access._
import choice.fs._
import choice.lib._
import choice.model.GlobalConfig
import net.liftweb.common._

case class UserDefinition(username : String, password : Option[String],
                          email : Option[String], regcode : Option[String], oauthonly : Boolean = false)

class CfsUserLib(principal : () ⇒ Principal, cfsFileLib : CfsFileLib) {

    def mkgroup(path : String, gdesc : Option[GroupDesc]) : Box[Map[String, Any]] = {
        cfsFileLib.getAbsolutePath(path) flatMap { gpath ⇒
            Cfs.ifNoFile(gpath, principal(), CfsOpenOptions.Default) { gp ⇒
                val parent = gp.getParent
                def doCreate(folder : CfsFolder) : Box[Map[String, Any]] = {
                    folder create (gp.getFileName.toString, GroupInfo.getMimeType, CfsCreateOptions.Default) match {
                        case Full(tginfo : GroupInfo) ⇒
                            gdesc foreach { tginfo.putData }
                            val map = tginfo.asMap
                            tginfo close ()
                            Full(map)
                        case Full(vfile) ⇒
                            vfile close ()
                            Failure(s"unexpected result from creating group ${gp.toString}")
                        case e : EmptyBox ⇒ e
                    }
                }
                Cfs.withExistingFile(parent, principal()) {
                    case pginfo : GroupInfo ⇒ doCreate(pginfo)
                    case folder : CfsFolder ⇒ GroupInfo.canCreateGroup (folder) { () ⇒
                        doCreate(folder)
                    }
                }
            }
        }
    }

    def mkuser(gpath : String, username : String, password : Option[String],
               email : Option[String], regcode : Option[String], oauthonly : Boolean = false) : Box[Map[String, Any]] = {
        cfsFileLib.getAbsolutePath(gpath) flatMap { gabspath ⇒
            Cfs.withExistingFile(gabspath, principal(), CfsOpenOptions.Default) {
                case ginfo : GroupInfo ⇒
                    ginfo addNewUser (username, password getOrElse "", email, regcode,
                                      pwdIsEncrypted = false, oauthOnly = oauthonly) map { uinfo ⇒
                        val umap : Map[String, Any] = uinfo.asMap + ("status" → 1)
                        uinfo close ()
                        umap
                    }
            }
        }
    }

    def mkusers(gpath : String, users : Seq[UserDefinition]) : Box[List[Map[String, Any]]] = {
        cfsFileLib.getAbsolutePath(gpath) flatMap { gabspath ⇒
            Cfs.withExistingFile(gabspath, principal(), CfsOpenOptions.Default) {
                case ginfo : GroupInfo ⇒
                    val rmaps = users map { udef ⇒
                        Option(udef) match {
                            case Some(_) ⇒
                                ginfo addNewUser(udef.username, udef.password getOrElse "", udef.email,
                                                 udef.regcode, pwdIsEncrypted = false,
                                                 oauthOnly = udef.oauthonly) match {
                                    case Full(uinfo) ⇒
                                        val result = uinfo.asMap
                                        uinfo close()
                                        result
                                    case e : EmptyBox ⇒
                                        Map[String, Any]("status" → -1,
                                                         "username" → udef.username, "msg" → (e ?~ "Empty").msg)
                                }
                            case None ⇒
                                Map[String, Any]("status" → -1, "msg" → "missing username")
                        }
                    }
                    Full(rmaps.toList)
            }
        }
    }

    /**
      * Find a user in a given group. The search may be recursive, in which case all the
      * descendant subgroups of the group are also searched. The caller is responsible
      * for closing the returned UserInfo object, if any. If the user is not found,
      * the result is Empty.
      *
      * @param group the group path, which may be relative to the current folder
      * @param username the username, which is folded to lower case for the search
      * @param recursive true if descendant groups should be searched
      * @return a boxed UserInfo object for the user if found, Empty if not found.
      */
    def findUser(group : String, username : String, recursive : Boolean) : Box[UserInfo] = {
        cfsFileLib.getAbsolutePath(group) flatMap { gabspath ⇒
            Cfs.withExistingFile(gabspath, principal()) {
                case ginfo : GroupInfo ⇒
                    if (recursive) ginfo findUser_! username
                    else ginfo findUser username
            }
        }
    }

    def listUsers(group : String, recursive : Boolean = false) : Box[List[UserInfo]] = {
        cfsFileLib.getAbsolutePath(group) flatMap { gabspath ⇒
            Cfs.withExistingFile(gabspath, principal()) {
                case ginfo : GroupInfo ⇒
                    Full(if (recursive) ginfo.getUsers_! else ginfo.getUsers)
            }
        }
    }

    def login(username : String, password : String,
              group : Option[String], gid : Option[Long]) : Box[SessionClient] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            val lcusername = username.toLowerCase
            UserOps.resolveGroup(group, gid, Some(sclient.getLoginGroup),
                                 SystemPrincipal, mustExist = true) flatMap { logingroup ⇒
                val somePwd = Option(password) orElse Some("")
                sclient.login(lcusername, somePwd, logingroup)
            }
        }
    }

    def logout() : Box[Boolean] = {
        SessionClient.withSessionState (requireLogin = true) { sclient ⇒
            sclient.logout map (_ ⇒ true)
        }
    }

    def getSession : Box[Map[String, Any]] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            Full(UserOps.getSession(sclient, principal()))
        }
    }

    def getLoginGroup : Box[String] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            Full(sclient.getLoginGroup)
        }
    }

    def isLoggedIn : Box[Boolean] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            Full(sclient.loggedIn_?)
        }
    }

    def checkUser(username : String, password : String,
                  group : Option[String], gid : Option[Long]) : Box[(Int, String)] = {
        SessionClient.withSessionState(requireLogin = false) { sclient ⇒
            UserOps.resolveGroup(group, gid, Some(sclient.getLoginGroup),
                SystemPrincipal, mustExist = true) flatMap { ginfo ⇒
                val lcusername = username.toLowerCase
                val pair = ginfo findUser_! lcusername match {
                    case Full(uinfo) ⇒
                        val valid = uinfo.validPassword_?(Some(password))
                        uinfo close ()
                        if (valid) (1, "valid username/password")
                        else (2, "invalid username/password")
                    case Empty ⇒ (0, "user does not exist")
                    case f : Failure ⇒ (-1, f.messageChain)
                }
                ginfo close ()
                Full(pair)
            }
        }
    }

    def canRegister(group : Option[String], gid : Option[Long]) : Box[Map[String, Boolean]] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            UserOps.resolveGroup(group, gid, Some(sclient.getLoginGroup),
                SystemPrincipal, mustExist = true) flatMap { ginfo ⇒
                val (signup, captcha, googleSignup, verifyEmail) = {
                    // If the first administrator hasn't registered, always allow registration
                    if (GlobalConfig.haveFirstAdmin) {
                        (ginfo.isSignupEnabled_?, ginfo.isCaptchaEnabled_?,
                            ginfo.isGoogleSignupEnabled_?, ginfo.isVerifyEmailEnabled_?)
                    }
                    else (true, false, true, false)
                }
                ginfo close ()
                Full(Map("status" → signup, "captcha" → captcha,
                         "googleSignup" → googleSignup, "verifyEmail" → verifyEmail))
            }
        }
    }

    /**
      * Reset the password for a user to a given new password. This requires the principal
      * to have "set_user_attr" rights to the user identity, and to be able to traverse
      * the path to the group containing the user. The principal for this operation
      * normally would be an administrator for the group. The user need not be logged
      * in, though for this to work, it would need to be invoked by a script running
      * as group administrator.
      *
      * @param username the username
      * @param newPassword the new password
      * @param group an optional group path for the group containing the user, defaulting
      *              to the current login group of the session
      * @return a boxed value of true if successful
      */
    def resetPassword(username : String, newPassword : String, group : Option[String]) : Box[Boolean] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            UserOps.resolveGroup(group, None, Some(sclient.getLoginGroup),
                                 principal(), mustExist = true) flatMap { ginfo ⇒
                val lcusername = username.toLowerCase
                val result =
                    (ginfo findUser lcusername) ?~ s"user $lcusername not found in group" flatMap { tuinfo ⇒
                        val resp = tuinfo setPassword newPassword map (_ ⇒ true)
                        tuinfo close ()
                        resp
                    }
                ginfo close ()
                result
            }
        }
    }
}
