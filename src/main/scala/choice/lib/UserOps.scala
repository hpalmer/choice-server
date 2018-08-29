/**
  * Copyright © 2013-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  *  Handle Ajax requests for user operations. Maintain session state.
  *
  * @author Howard Palmer
  *
  */
package choice.lib

import java.security.PublicKey

import _root_.net.liftweb._
import net.liftweb.http.provider.HTTPCookie
import util._
import Helpers._
import http._
import common._
import json._
import choice.lib.JsonHelpers._
import choice.access._
import choice.actor.SessionManager
import choice.model._
import net.tanesha.recaptcha.ReCaptchaImpl
import net.liftweb.mapper.By
import choice.fs._
import choice.core.{LogFilter, Startup}
import choice.lib.ExtendedBox._
import net.liftweb.http.LiftRules.DispatchPF

import scala.collection.mutable

/** Handle user Ajax operations */
object UserOps extends Factory {

    implicit val formats : DefaultFormats.type = DefaultFormats
    
    private val Log = Logger("choice.lib.UserOps")


  /** Set up to receive Ajax posts to "user" */
    def init() : Unit = {
         val rules : DispatchPF = {
            // register the user operations handler
            case r @ Req(_, _, PostRequest) if r.param("api") == Full("user") ⇒ () ⇒ handleOp(r)
        }
        LiftRules.dispatch.prepend(rules)
    }

    def resolveGroup(group : Option[String], gid : Option[Long], defgroup : Option[String],
                     principal : Principal, mustExist : Boolean = true) : Box[GroupInfo] = {
        val groupInfo = Box(group) flatMap { gpath ⇒
            Cfs open (gpath, principal, CfsOpenOptions.Default) match {
                case Full(g : GroupInfo) ⇒ Full(g)
                case Full(other) ⇒
                    val opath = other.getPath.toString
                    other close ()
                    Failure(s"$opath is not a group")
                case Empty ⇒
                    if (mustExist) Failure(s"$gpath does not exist")
                    else Empty
                case f : Failure ⇒ f
            }
        }
        val gidInfo = Box(gid) flatMap { id ⇒
            Cfs open (CfsVFileId(id), principal, CfsOpenOptions.Default) match {
                case Full(g : GroupInfo) ⇒ Full(g)
                case Full(other) ⇒
                    val opath = other.getPath.toString
                    other close ()
                    Failure(s"$opath is not a group")
                case Empty ⇒
                    if (mustExist) Failure(s"group id $id does not exist")
                    else Empty
                case f : Failure ⇒ f
            }
        }
        (group, gid) match {
            case (Some(_), Some(_)) ⇒
                gidInfo flatMap { g1 ⇒
                    groupInfo flatMap { g2 ⇒
                        if (g1.getResourceId == g2.getResourceId) {
                            g1 close ()
                            Full(g2)
                        }
                        else {
                            g1 close ()
                            g2 close ()
                            Failure(s"inconsistent group specification")
                        }
                    }
                }
            case (Some(_), None) ⇒ groupInfo
            case (None, Some(_)) ⇒ gidInfo
            case (None, None) ⇒
                defgroup match {
                    case Some(_) ⇒ resolveGroup(defgroup, None, None, principal, mustExist)
                    case None ⇒ Failure("no group specified")
                }
        }
    }

    def getSession(sclient : SessionClient, principal : Principal) : Map[String, Any] = {
        // Collect information about the user and associated groups
        val ugattr = (sclient getUserInfo SystemPrincipal).toList flatMap { uinfo ⇒
            val ginfo = uinfo.getGroupInfo
            val home = ginfo.getHomePage or Full(Startup.DefaultHomePage)

            // Add to the map an empty set of roles for any groups to which the user belongs,
            // without any assigned roles
            val grouplist : List[(String, GroupInfo)] = uinfo.getGroupList

            // Now generate the "groups" attribute, an array of maps, each containing
            // information about the user's relationship to one group
            val groups = grouplist map {
                case (username, ugroup) ⇒
                    val gmap : Map[String, Any] = {
                        Map("gid" → ugroup.getFileId.resource.id,
                            "name" → ugroup.getName,
                            "desc" → ugroup.getDescription,
                            "member" → username,
                            "paths" → ugroup.findPaths.map(_.toString),
                            "roles" → (if (GlobalConfig.isSystemAdmin_?(sclient.getPrincipal)) List("ADMIN") else Nil)
                        )
                    }
                    ugroup close ()
                    gmap
            }
            val user = Map[String, Any]("name" → uinfo.getUsername,
                "id" → uinfo.getSelfPrincipalId.id,
                "path" → uinfo.getPath.toString)
            val cgid = ginfo.getFileId.resource.id
            uinfo close ()
            List("sessionid" → sclient.sessionId.id,
                "cgid" → cgid,
                "logingroup" → sclient.getLoginGroup,
                "logintype" → sclient.getLoginType,
                "user" → user,
                "home" → home.toOption,
                "groups" → groups,
                "contextPath" → LiftRules.context.path)
        }
        Map(ugattr : _*)
    }

    /**
     * This request serves two purposes:
     *      - provide the client with information about the current session
     *      - log a reference to a specified page (generally the page issuing the request)
     *
     * @param page page URI, normally the current page
     */
    sealed case class AjaxGetSession(page : String) extends AjaxApiRequest("user", "getsession") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            //    Log a page hit for the page
//            sclient logPageHit page

            MapResponse(getSession(sclient, self))
        }
    }

    def getHomePage(sclient : SessionClient) : Option[String] = {
        val boxpage = sclient getGroupInfo SystemPrincipal flatMap { ginfo ⇒
            Log.info(s"getHomePage: group=${ginfo.getPath.toString}")
            val pagestr = ginfo.getHomePage match {
                case full @ Full(page) ⇒
                    Log.info(s"getHomePage: home page=$page")
                    full
                case Empty ⇒
                    val page = Startup.DefaultHomePage
                    Log.info(s"getHomePage: default home page=$page")
                    Full(page)
                case f : Failure ⇒
                    Log.error(s"getHomePage: ${f.messageChain}")
                    f
            }
            ginfo close ()
            pagestr
        }
        boxpage.toOption
    }

    object SessionTokenManager {

        case class Entry(jsessionid : String, path : String, timestamp : Long = millis)

        private val sessionTokens = mutable.Map[String, Entry]()

        /**
         * Generate a session token for the current session, and store it.
         *
         * @return the token string
         */
        def get : String = {
            import scala.language.postfixOps
            val token = randomString(32)
            S.session.map(_.uniqueId) foreach { jsessionid ⇒
                val path = {
                    val cpath = S.contextPath
                    if (cpath.endsWith("/")) cpath else cpath + "/"
                }
                val entry = Entry(jsessionid, path)
                synchronized {
                    sessionTokens put (token, entry)
                }
            }
            // Remove this token in the not-too-distant future
            Schedule.schedule(() ⇒ {
                synchronized {
                    sessionTokens remove token
                }
            }, 60 seconds)
            token
        }

        def set(token : String) : Option[HTTPCookie] = synchronized {
            sessionTokens get token map { entry ⇒
                HTTPCookie(Startup.jSessionIdName, Full(entry.jsessionid),
                           Empty, Full(entry.path), Empty, Empty, Empty, Full(true))
            }
        }
    }

    sealed case class AjaxPassSession(cmd : String, token : Option[String]) extends AjaxApiRequest("user", "pass") {

        implicit val formats = DefaultFormats
        import json.Extraction.decompose

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (cmd == "get") {
                val token = SessionTokenManager.get
                MapResponse(Map("status" → 1, "token" → token))
            }
            else if (cmd == "set") {
                token match {
                    case Some(t) ⇒
                        SessionTokenManager.set(t) match {
                            case Some(scookie) ⇒
                                // Logout the current session, but only if it's not the one
                                // associated with the JSESSIONID we're about to set.
                                for (newSessionId ← scookie.value;
                                     curSessionId ← S.session.map(_.uniqueId) if newSessionId != curSessionId) {
                                    sclient.logout
                                }
                                val rmap = decompose(Map("status" → 1))
                                val cookies = scookie :: (S.responseCookies filterNot (_.name.startsWith("JSESSIONID")))
                                Full(JsonResponse(rmap, S.getResponseHeaders(Nil), cookies, 200))
                            case None ⇒ Failure("invalid session token")
                        }
                    case None ⇒ Failure("missing token for session pass operation")
                }
            }
            else Failure(s"unrecognized session pass command '$cmd'")
        }
    }

    /**
     * Create an entry in the database for a specified web page.
     *
     * @param page the path to the page within the current application context
     * @return a `PageDef` instance for the page
     */
    def createPageDef(page : String) : PageDef = {
        PageDef.find(By(PageDef.page, page)) openOr PageDef.create.page(page).saveMe
    }

    sealed case class AjaxLoginUser(username : String, password : Option[String],
                                    group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "login") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // If no group is specified, use the login group from the session
            val loginGroup = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
            loginGroup flatMap { ginfoLogin ⇒
                val lcusername = username.toLowerCase
                val pwdstring = password getOrElse ""
                val result : Box[SessionClient] =
                    // Somebody already logged in?
                    if (sclient.loggedIn_?) {
                        val currentUsername = sclient.getPrincipal.getPrincipalName
                        // Yes, see if the specified username/password is valid
                        ginfoLogin findUser_! lcusername match {
                            case Full(uinfo) if uinfo.getSelfPrincipalId == self.getPrincipalId ⇒
                                // Already logged in as the specified user, but is the password correct?
                                val pwdOk = uinfo validPassword_? password
                                uinfo close ()
                                if (pwdOk) {
                                    // Yes, just continue the login session as the current user
                                    Log.info(lcusername + " is already logged in")
                                    Full(sclient)
                                }
                                else {
                                    // The current user either mistyped or doesn't know their own password.
                                    // Log them out, just in case it isn't really the current user.
                                    Log.warn(s"logging out $currentUsername due to failed re-login attempt")
                                    sclient.logout
                                    Failure("invalid username/password")
                                }
                            case Full(uinfo) ⇒
                                // Already logged in as some other user. Terminate the session, logging
                                // out that user in the process.
                                uinfo close ()
                                Log.info(s"logging out $currentUsername to login $username")
                                S.session foreach SessionManager.terminateSession
                                // Make a new session and use it to login the specified user.
                                SessionClient.withSessionState(requireLogin = false) { nclient ⇒
                                    // (This could fail, of course.)
                                    nclient.login(lcusername, Some(pwdstring), ginfoLogin)
                                }
                            case Empty ⇒
                                Log.warn(s"logging out $currentUsername because of attempt to login $username")
                                sclient.logout
                                Failure("invalid username/password")
                            case f : Failure ⇒
                                Log.warn(s"logging out $currentUsername due to failed attempt to login $username")
                                sclient.logout
                                f
                        }
                    }
                    else sclient.login(lcusername, Some(pwdstring), ginfoLogin)
                Log.info(s"AjaxLoginUser: result = $result")
                ginfoLogin close ()
                result match {
                    case Full(nclient) ⇒
                        val roles =
                            if (GlobalConfig.isSystemAdmin_?(nclient.getPrincipal)) Full("ADMIN") else Empty
                        val page = getHomePage(nclient)
                        val respmap = Map[String, Any]("status" → 1, "role" → roles.toList, "redirect" → page)
                        MapResponse(respmap)
                    case f : Failure ⇒ FailureResponse(f)
                    case Empty ⇒ ImpossibleResponse
                }
            }
        }
    }

    sealed case class AjaxLogoutUser() extends AjaxApiRequest("user", "logout") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            sclient.logout flatMap (_ ⇒ SuccessResponse)
        }
    }

    sealed case class AjaxLoginGroupInfo() extends AjaxApiRequest("user", "lginfo") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            resolveGroup(Some(sclient.getLoginGroup), None, None, SystemPrincipal, mustExist = true) flatMap { ginfo ⇒
                val result = ginfo.getDataUnchecked flatMap { gdesc ⇒
                    MapResponse(gdesc.asMap)
                }
                ginfo close ()
                result
            }
        }
    }

    sealed case class AjaxCheckRegKey(key : String) extends AjaxApiRequest("user", "regkey") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            resolveGroup(Some(sclient.getLoginGroup), None, None, SystemPrincipal, mustExist = true) flatMap { ginfo ⇒
                val result = ginfo.getDataUnchecked flatMap { gdesc ⇒
                    gdesc.regKey match {
                        case Some(regkey) ⇒ MapResponse(Map("status" → (if (regkey.equalsIgnoreCase(key)) 1 else 0)))
                        case None ⇒ Failure(s"${sclient.getLoginGroup} does not have a registration key")
                    }
                }
                ginfo close ()
                result
            }
        }
    }

    sealed class AjaxCanRegister(group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "register?") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // If no group is specified, use the login group from the session
            val gbox = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
            gbox flatMap { rginfo ⇒
                val (signup, captcha) = {
            		// If the first administrator hasn't registered, always allow registration
                    if (GlobalConfig.haveFirstAdmin) (rginfo.isSignupEnabled_?, rginfo.isCaptchaEnabled_?)
                    else (true, false)
                }
                rginfo close ()
                MapResponse(Map("status" → signup, "captcha" → captcha))
            }
        }
    }

    /**
     * This is used mainly to check the validity and availability of a username during self-registration.
     * It also used to verify an optional password in the case where the username already exists. But
     * that functionality seems like an unnecessary security risk, so any password is now ignored.
     *
     * The group in which the username is to be checked can be specified explicitly by its file path
     * or group id. If both are specified, they must refer to the same group. If neither are specified,
     * the current login group associated with the session is used.
     *
     * In order for a username to be available, it must not exist anywhere in any group hierarchy
     * to which the indicated group belongs. This is because users are considered to be members of
     * all ancestor groups of the group in which they are defined. This operation checks for the
     * existence of the username in both ancestor and descendant groups of the indicated group, to
     * ensure that a username will always identify a unique user in any given directed acyclic graph
     * of user groups. The same username can be used for different users in groups which are unrelated.
     *
     * @param username the username to validate for syntax and availability
     * @param password an optional password, now completely ignored
     * @param group an optional group path for a user group
     * @param gid an optional group id of a group
     */
    sealed class AjaxValidateUser(username : String, password : Option[String],
                                  group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "validate") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // Check the syntax of the username (Empty indicates no syntax errors)
            UserInfo validateUsername username match {
                case Empty ⇒
                    // If no group is specified, use the login group from the session
                    val gbox = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
                    gbox flatMap { vginfo ⇒
                        // See if the username already exists within the group DAG
                        val taken = vginfo.getVnode isNameUsed_? (username.toLowerCase, None)
                        vginfo close ()
                        if (taken) {
                            val pwdignored = password map (_ ⇒ s" (password ignored)") getOrElse ""
                            SimpleResponse(2, s"username exists$pwdignored")
                        }
                        else SimpleResponse(0, "username does not exist")
                    }
                case f : Failure ⇒ f
            }
        }
    }

    sealed class AjaxRegisterUser(username : String, password : String,
                                  group : Option[String], gid : Option[Long], email : Option[String],
                                  regcode : Option[String], recaptcha_challenge_field : Option[String],
                                  recaptcha_response_field : Option[String])
        extends AjaxApiRequest("user", "register") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // If no group is specified, use the login group from the session
            // TODO: self-registered users are always owned by the SystemPrincipal, but the owner probably
            // should be the owner of the user group.
            val gbox = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
            gbox flatMap { pginfo ⇒
                val haveFirstAdmin = GlobalConfig.haveFirstAdmin
                val result =
                    if (haveFirstAdmin && !pginfo.isSignupEnabled_?) Failure("Registration is disabled.")
                    else {
                        val useCaptcha = pginfo.isCaptchaEnabled_?
                        if (useCaptcha && List(recaptcha_challenge_field, recaptcha_response_field).exists(_.isEmpty)) {
                            return MissingParameter("register")
                        }
                        val remoteAddr = req.remoteAddr
                        val valid = if (pginfo.isCaptchaEnabled_?) {
                            val reCaptcha = new ReCaptchaImpl

                            // *** YOU MUST PROVIDE YOUR PRIVATE KEY HERE ***
                            // Probably don't want to hardcode your private key here but just to
                            // get it working is OK...
                            reCaptcha.setPrivateKey(
                                "6Lc1Ab8SAAAAAOH3JwlOWA4bEcs4LqlUWCEg39Sa")

                            val reCaptchaResponse =
                                reCaptcha.checkAnswer(remoteAddr, recaptcha_challenge_field.get,
                                    recaptcha_response_field.get)
                            reCaptchaResponse.isValid
                        }
                        else true
                        var respmap : Map[String, Any] = Map("valid" → valid)
                        if (valid) {
                            val newuser = pginfo addNewUser (username, password, email, regcode,
                                                             pwdIsEncrypted = false, oauthOnly = false)
                            newuser match {
                                case Full(nuinfo) ⇒
                                    // The first user to register is made administrator
                                    if (!haveFirstAdmin) {
                                        Cfs.withExistingFile(Startup.AdminUsersPath, SystemPrincipal, CfsOpenOptions.Default) {
                                            case admgroup : GroupInfo ⇒
                                                pginfo copyUsers (admgroup, List(username.toLowerCase), None) foreach {
                                                    _._2 foreach (_ close ())
                                                }
                                                Empty
                                        }
                                    }
                                    // Set the owner of a self-registered user to be the owner of the user group
                                    // in which they are registered.
                                    val groupOwner = pginfo.getVnode.getOwnerId
                                    if (groupOwner != nuinfo.getVnode.getOwnerId) {
                                        // chown closes nuinfo and returns a new handle
                                        nuinfo chown CfsPrincipal(groupOwner) foreach (_ close ())
                                    }
                                    else {
                                        nuinfo close()
                                    }
                                    respmap = respmap + ("status" → 0, "msg" → "ok")
                                case f : Failure ⇒ respmap = respmap + ("status" → -1, "msg" → f.msg)
                                case Empty ⇒ respmap = respmap + ("status" → -1, "msg" → "createUser returned Empty")
                            }
                        }
                        else {
                            respmap = respmap + ("status" → -1, "msg" → "captcha failed")
                        }
                        Full(respmap)
                    }
                pginfo close ()
                result flatMap MapResponse
            }
        }
    }

    sealed case class AjaxMakeUser(username : String, password : Option[String],
                                   group : Option[String], gid : Option[Long],
                                   email : Option[String], regcode : Option[String], oauthonly : Option[Boolean])
        extends AjaxApiRequest("user", "mkuser") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // There is no default group for this operation
            val gbox = resolveGroup(group, gid, None, self, mustExist = true)
            gbox flatMap { nginfo ⇒
                val passwd = password getOrElse UserInfo.generatePassword()
                val result = nginfo addNewUser (username, passwd, email, regcode,
                                                pwdIsEncrypted = false,
                                                oauthOnly = oauthonly getOrElse false) flatMap { nuinfo ⇒
                    val respmap : Map[String, Any] =
                        Map("status" → 1,
                            "id" → nuinfo.getFileId.resource.id,
                            "username" → nuinfo.getUsername,
                            "password" → passwd,
                            "path" → nuinfo.getPath.toString)
                    nuinfo close()
                    Full(respmap)
                }
                nginfo close ()
                result flatMap MapResponse
            }
        }
    }
    
    /**
     * Change password of the current user.
     */
    case class AjaxChangePassword(oldpwd : Option[String], newpwd : Option[String])
        extends AjaxApiRequest("user", "setpwd") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            sclient getUserInfo SystemPrincipal flatMap { uinfoCurrent ⇒
                val result =
                    if (uinfoCurrent validPassword_? oldpwd) uinfoCurrent setPassword (newpwd getOrElse "")
                    else Failure("current password is invalid")
                uinfoCurrent close ()
                result flatMap (_ ⇒ SuccessResponse)
            }
        }
    }

    case class AjaxResetPassword(userid : String, newpwd : String,
                                 group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "resetpwd") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // There is no default group for this operation
            val gbox = resolveGroup (group, gid, None, self, mustExist = true)
            gbox flatMap { tginfo ⇒
                val lcusername = userid.toLowerCase
                val result =
                    (tginfo findUser lcusername) ?~ s"user $lcusername not found in group" flatMap { tuinfo ⇒
                        val resp = tuinfo setPassword newpwd flatMap (_ ⇒ SuccessResponse)
                        tuinfo close ()
                        resp
                     }
                tginfo close ()
                result
            }
        }
    }

    private var googleKeysCache : Box[(Long, Map[String, PublicKey])] = Empty

    private def googleKeys = {
        val update = googleKeysCache.isEmpty || googleKeysCache.map(millis - _._1 > 20.minutes.millis).openOr(true)
        if (update) {
            def helper(i : Int) : Box[(Long, Map[String, PublicKey])] = {
                OAuthHelpers.getGooglePublicKeys match {
                    case Full(keymap) ⇒ Full((millis, keymap))
                    case e : EmptyBox ⇒ if (i < 2) helper(i + 1) else e
                }
            }
            googleKeysCache = helper(0)
        }
        googleKeysCache.map(_._2).openOr(Map())
    }

    case class AjaxGoogleLogin(token_id : String, username : Option[String], password : Option[String],
                               group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "glogin") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // If no group is specified, use the login group from the session
            val gbox = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
            val sclientbox = gbox flatMap { ginfo ⇒
                // Validate the Google token, extract the email address, and see if a corresponding
                // users exists.
                val tokenInfoBox = OAuthHelpers.parseWebToken(token_id, googleKeys) flatMap { token ⇒
                    (token.claims.get("email"), token.claims.get("email_verified")) match {
                        case (Some(email : String), Some(true)) ⇒
                            ginfo.findUser_!(email) match {
                                case Full(uinfo) ⇒ Full((email, Some(uinfo)))
                                case Empty ⇒ Full((email, None))
                                case f : Failure ⇒ f
                            }
                        case (Some(_ : String), Some(false)) ⇒ Failure("email address is not verified")
                        case (Some(_), _) ⇒ Failure("email address is not a string")
                        case _ ⇒ Failure("invalid credentials in web token")
                    }
                }
                val currentUsername = sclient.getPrincipal.getPrincipalName
                // Helper function to logout current user and login a specified user
                def helper(username : String, gdesc : GroupDesc) : Box[SessionClient] = {
                    if (sclient.loggedIn_?) {
                        // Already logged in as some other user. Terminate the session, logging
                        // out that user in the process.
                        Log.info(s"logging out $currentUsername to login $username")
                        S.session foreach SessionManager.terminateSession
                    }
                    if (gdesc.isGoogleLoginEnabled_?) {
                        // Make a new session and use it to login the specified user.
                        SessionClient.withSessionState(requireLogin = false) { nclient ⇒
                            // (This could fail, of course.)
                            nclient.login(username.toLowerCase, password, ginfo) use (_.setLoginType("google"))
                        }
                    }
                    else Failure("Google logins are not permitted at this time.")
                }
                // Get the settings for the group
                val result = ginfo.getDataUnchecked flatMap { gdesc ⇒
                    tokenInfoBox flatMap {
                        case (email, Some(uinfo)) ⇒
                            // The user exists. Check if already logged in.
                            val loggedIn = sclient.loggedIn_? && uinfo.getSelfPrincipalId == self.getPrincipalId
                            uinfo close ()
                            if (loggedIn) {
                                // Yes, do nothing
                                Full(sclient)
                            }
                            else {
                                // Else logout the current user if any, and login the Google user
                                helper(email, gdesc)
                            }
                        case (email, None) ⇒
                            // The Google token contained a verified email address, but the user
                            // does not exist. See if new Google signups are allowed.
                            if (gdesc.isGoogleSignupEnabled_?) {
                                username match {
                                    case Some(uname) ⇒
                                        // A username was also specified. If the username/password validate to
                                        // an existing user in the group, make the email address an alias for
                                        // that user. Otherwise the signup fails.
                                        ginfo.validateUser(uname, password orElse Some("")) flatMap { exuinfo ⇒
                                            val nsclientbox = ginfo link (email.toLowerCase, exuinfo) flatMap { emuinfo ⇒
                                                emuinfo close ()
                                                if (gdesc.isGoogleLoginEnabled_?) {
                                                    helper(email, gdesc)
                                                }
                                                else Failure(s"Linked $email to $uname, but Google logins are not permitted at this time")
                                            }
                                            exuinfo close ()
                                            nsclientbox
                                        }
                                    case None ⇒
                                        // Create a new user, using the email address as the username
                                        ginfo.addNewUser(email, password.getOrElse(""),
                                                         Some(email), None, oauthOnly = true) flatMap { uinfo ⇒
                                            uinfo close ()
                                            if (gdesc.isGoogleLoginEnabled_?) {
                                                helper(email, gdesc)
                                            }
                                            else Failure("User created, but Google logins are not permitted at this time.")
                                        }
                                }
                            }
                            else Failure(s"User $email does not exist, and Google signups are not permitted at this time")
                    }
                }
                ginfo close ()
                result
            }
            sclientbox match {
                case Full(nclient) ⇒
                    val roles =
                        if (GlobalConfig.isSystemAdmin_?(nclient.getPrincipal)) Full("ADMIN") else Empty
                    val page = getHomePage(nclient)
                    val respmap = Map[String, Any]("status" → 1, "role" → roles.toList, "redirect" → page)
                    MapResponse(respmap)
                case f : Failure ⇒ FailureResponse(f)
                case Empty ⇒ ImpossibleResponse
            }
        }
    }

    private val fbsecret = new FactoryMaker[String](Props.get("facebook.secret", "")){}

    case class AjaxFacebookLogin(authstring : String, email : Option[String], regcode : Option[String],
                                 group : Option[String], gid : Option[Long])
        extends AjaxApiRequest("user", "fblogin") {

        import javax.crypto.Mac
        import javax.crypto.spec.SecretKeySpec

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {

            // If no group is specified, use the login group from the session
            val gbox = resolveGroup(group, gid, Some(sclient.getLoginGroup), SystemPrincipal, mustExist = true)
            gbox flatMap { ginfo ⇒
                val result = getCredentials(authstring) flatMap {
                    case (username, password) ⇒
                        val lcusername = username.toLowerCase
                        val gotuser = ginfo.withMember (lcusername) {
                            case _ : UserInfo ⇒ Full(true)
                        }
                        val haveuser = gotuser match {
                            case full @ Full(_) ⇒ full
                            case Empty ⇒
                                val options = UserCreateOptions(username = Some(username), password = Some(password),
                                                                email = email, regcode = regcode)
                                ginfo create (lcusername, UserInfo.getMimeType, options) flatMap { nuinfo ⇒
                                    nuinfo close ()
                                    Full(true)
                                }
                            case f : Failure ⇒ f
                        }
                        haveuser flatMap { _ ⇒
                            val ajaxLogin = AjaxLoginUser (lcusername, Some(password), group, gid)
                            ajaxLogin getResponse (req, sclient, self)
                        }
                }
                ginfo close ()
                result
            }
        }

        def getCredentials(authstring : String) : Box[(String, String)] = {
            (authstring split '.').toList match {
                case signature :: payload :: Nil ⇒
                    val result = tryo {
                        val sigbytes = base64Decode (signature replaceAllLiterally("-", "+") replaceAllLiterally("_", "/"))
                        val json = JsonParser parse new String(base64Decode(payload))
                        json \ "algorithm" match {
                            case JString(algorithm) ⇒
                                if (algorithm equalsIgnoreCase "HMAC-SHA256") {
                                    Log.info(s"facebook signed request json: ${prettyRender(json)}")
                                    val mac = Mac.getInstance("HmacSHA256")
                                    mac.init(new SecretKeySpec(fbsecret.vend.getBytes, "HmacSHA256"))
                                    val mysig = mac.doFinal(payload.getBytes)
                                    if (java.util.Arrays.equals(mysig, sigbytes)) {
                                        val JString(user_id) = json \ "user_id"
                                        (s"FB$user_id", "TODO")
                                    }
                                    else throw new SecurityException ("invalid Facebook signature")
                                }
                                else throw new SecurityException (s"Unknown algorithm $algorithm. Expected HMAC-SHA256")
                            case _ ⇒
                                throw new SecurityException("algorithm is not a string")
                        }

                    }
                    result
                case _ ⇒ Failure("authstring did not contain exactly one '.'")
            }
        }
    }

    case class AjaxSessionLog(enable : Boolean, all : Option[Boolean]) extends AjaxApiRequest("user", "slog") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            all match {
                case Some(b) ⇒
                    if (self.isSystemAdmin_?) {
                        SessionManager.logAllSessions(b)
                        if (!b) {
                            LogFilter.disableLogging(None)
                        }
                        SuccessResponse
                    }
                    else Failure("requires system administrator")
                case None ⇒
                    if (enable) {
                        LogFilter.enableLogging(sclient.jSessionId)
                    }
                    else {
                        LogFilter.disableLogging(Some(sclient.jSessionId))
                    }
                    SuccessResponse
            }
        }
    }

    val optable : Map[String, AjaxApiRequestExtractor[AjaxRequest]] = Map(
        "getsession" → AjaxApiRequestExtractor[AjaxGetSession]("user", "getsession"),
        "pass" → AjaxApiRequestExtractor[AjaxPassSession]("user", "pass"),
        "login" → AjaxApiRequestExtractor[AjaxLoginUser]("user", "login"),
        "logout" → AjaxApiRequestExtractor[AjaxLogoutUser]("user", "logout"),
        "validate" → AjaxApiRequestExtractor[AjaxValidateUser]("user", "validate"),
        "mkuser" → AjaxApiRequestExtractor[AjaxMakeUser]("user", "mkuser"),
        "register" → AjaxApiRequestExtractor[AjaxRegisterUser]("user", "register"),
        "register?" → AjaxApiRequestExtractor[AjaxCanRegister]("user", "register?"),
        "setpwd" → AjaxApiRequestExtractor[AjaxChangePassword]("user", "setpwd"),
        "resetpwd" → AjaxApiRequestExtractor[AjaxResetPassword]("user", "resetpwd"),
        "glogin" → AjaxApiRequestExtractor[AjaxGoogleLogin]("user", "glogin"),
        "lginfo" → AjaxApiRequestExtractor[AjaxLoginGroupInfo]("user", "lginfo"),
        "regkey" → AjaxApiRequestExtractor[AjaxCheckRegKey]("user", "regkey"),
        "fblogin" → AjaxApiRequestExtractor[AjaxFacebookLogin]("user", "fblogin"),
        "slog" → AjaxApiRequestExtractor[AjaxSessionLog]("user", "slog")
    )

    val requestExtractor = new AjaxRequestTable(optable)

    /** Handle a user operation Ajax request */
    def handleOp(req : Req) : Box[LiftResponse] = {

        requestExtractor.getReq(req) match {
            case Full(ajax) ⇒ ajax processRequest req
            case f : Failure ⇒ FailureResponse(f)
            case Empty ⇒ SimpleResponse(-1, "missing user operation")
        }
    }

}
