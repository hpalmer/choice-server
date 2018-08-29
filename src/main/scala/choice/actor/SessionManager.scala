/**
  * Copyright © 2013-2018 The Board of Trustees of The Leland Stanford Junior University.
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
  * An actor which manages HTTP sessions with the server, including login/logout.
  *
  * @author Howard Palmer
  */
package choice.actor

import java.util.concurrent.atomic.AtomicBoolean
import javax.servlet.{ServletContextEvent, ServletContextListener}

import _root_.choice.model._
import choice.access._
import choice.core.{LogFilter, Startup}
import choice.fs._
import choice.lib.{FileOps, SessionClient}
import net.liftweb.actor._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.collection.mutable

class ShutdownListener extends ServletContextListener {
    import ShutdownListener._
    override def contextInitialized(scevent : ServletContextEvent) : Unit = {
        shutdown = false
        System.out.println("ShutdownListener: contextInitialized called")
    }

    override def contextDestroyed(scevent : ServletContextEvent) : Unit = {
        shutdown = true
        System.out.println("ShutdownListener: contextDestroyed called")
    }
}

object ShutdownListener {
    var shutdown = false
}

/**
 * The SessionManager tracks all the active sessions on the server.
 * 
 * The SessionManager runs as an actor, and other actors may register to
 * listen for session events, such as session start/end and login/logout.
 * It also provides information on request via messages.
 */
object SessionManager extends LiftActor with ListenerManager {
    private val Log = Logger("choice.actor.SessionManager")
    
    private val thePages = collection.mutable.Map[String, PageDef]()

    private val allSessionLog : AtomicBoolean = new AtomicBoolean(false)

    def logAllSessions(enable : Boolean) : Unit = {
        allSessionLog.set(enable)
    }

    def getSessionInfo(jsessionid : String) : Box[SessionClient] = {
        SessionManager !! JSessionInfoRequest(jsessionid) match {
            case Full(sinfo : SessionClient) ⇒ Full(sinfo)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from getSessionInfo")
        }
    }

    def getSessionInfo(id : ResourceId) : Box[SessionClient] = {
        SessionManager !! SessionInfoRequest(id) match {
            case Full(sinfo : SessionClient) ⇒ Full(sinfo)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from getSessionInfo")
        }
    }

    /**
     * Basic information about an active HTTP session.
     */
    private class SessionState(session : LiftSession,
                               principal : CfsPrincipal,
                               sfile : SessionFolder,
                               summary : SessionSummaryV1) extends SessionClient {

        private var _loggedIn_? : Boolean = false

        private var _loginType : String = "normal"
        
        private var _principal : CfsPrincipal = principal

        private var _loginGroup : String = Startup.DefaultUsersPath

        private var _sessionFolder = sfile

        private var _sessionSummary = summary

        /** The HTTP session id of this session */
        def jSessionId : String = summary.sessionId
    
        /** The associated Lift session instance */
        val liftSession : LiftSession = session

        /** The client IP address */
        def clientIpAddress : String = summary.ipAddress

        /** The client browser's user agent string */
        def userAgent : String = summary.userAgent

        /** Session start time (server) */
        def startTime : Long = summary.startTime

//        /**
//         * Clock skew between client and server. Add this to client times to
//         * get server times.
//         */
//        def clockSkew = webSession.flatMap { ws ⇒
//            Full(if (ws.tstamp.is == 0) 0L else ws.startTime.is - ws.tstamp.is)
//        } openOr 0L
//
//        def setClientTime(tstamp : Long) : Box[SessionClient] = {
//            webSession.foreach(_.setClientTime(tstamp))
//            Full(this)
//        }
        
        /** Terminate the session */
        def terminate() : Unit = {
            for ((_, act) ← _activities.toList) act close ()
            getSessionFolder close ()
        }

        def sessionId : ResourceId = sfile.getResourceId

        def getPrincipal : CfsPrincipal = _principal

        def setPrincipal(principal : CfsPrincipal) : Unit = { _principal = principal }

        /**
         * Get the pathname of the login group, initially the default user group.
         * The login group is only used when the user logs in, as the group assumed
         * to contain the username given for the login. It is ignored if the login
         * user is specified as a full path.
         *
         * @return the login group file path
         */
        def getLoginGroup : String = _loginGroup

        /**
         * Set the login group.
         *
         * @param gpath the group file path
         * @return true if the group exists. Otherwise false and the login group is set
         *         to the default user group.
         */
        def setLoginGroup(gpath : String) : Boolean = {
            Cfs.withExistingFile (gpath, SystemPrincipal, CfsOpenOptions.Default) {
                case _ : GroupInfo ⇒
                    _loginGroup = gpath
                    Full(true)
            } openOr {
                _loginGroup = Startup.DefaultUsersPath
                false
            }
        }

        /** Is the user logged in? */
        def loggedIn_? : Boolean = _loggedIn_?
        
        /**
          * Get the type of login. This will be "normal" unless an external account,
          * such as Google or Facebook is used, in which case it would be "google"
          * or "facebook", respectively.
          *
          * @return the login type, or "none" if the user is not logged in
          */
        override def getLoginType : String = if (loggedIn_?) _loginType else "none"

        /**
          * Set the type of login. This should only be necessary if an external account,
          * such as Google or Facebook is used.
          *
          * @param loginType the login type, e.g. "google" or "facebook"
          */
        override def setLoginType(loginType : String) : Unit = {
            _loginType = loginType
        }

        def getUserInfo(principal : Principal) : Box[UserInfo] = {
            val ppath = getPrincipal.getPrincipalPath
            Cfs open (ppath, principal, CfsOpenOptions.Default) match {
                case Full(uinfo : UserInfo) ⇒ Full(uinfo)
                case Full(other) ⇒
                    other close ()
                    Failure(s"$ppath is not a user")
                case Empty ⇒ Failure(s"$ppath does not exist")
                case f : Failure ⇒ f
            }
        }

        def getGroupInfo(principal : Principal) : Box[GroupInfo] = {
            val ppath = getPrincipal.getPrincipalPath
            Cfs open (ppath.getParent, principal, CfsOpenOptions.Default) match {
                case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
                case Full(other) ⇒
                    other close ()
                    Failure(s"$ppath is not a group")
                case Empty ⇒ Failure(s"$ppath does not exist")
                case f : Failure ⇒ f
            }
        }

        /**
          * Login the given username of the specified group. Note that if the password is not
          * specified (i.e. is None), no password checking is done. That variation is typically
          * used when authentication has already been achieved through some other means. If
          * authentication has not already been done, a password should always be provided,
          * even if only Some("").
          *
          * @param username the username, which is case-insensitive
          * @param password the optional password
          * @param ginfo file handle for the user's group
          * @return a boxed SessionClient if successful
          */
        def login(username : String, password : Option[String], ginfo : GroupInfo) : Box[SessionClient] = {
            SessionManager.getSessionState(create = true) match {
                case Some(state) ⇒
                    val req = S.request
                    SessionManager !! LoginUserRequest(state, req, username, password, ginfo) match {
                        case Full(sclient : SessionClient) ⇒ Full(sclient)
                        case Full(e : EmptyBox) ⇒ e
                        case _ ⇒ Failure("unexpected result from LoginUserRequest")
                    }
                case None ⇒ Failure("no session")
            }
        }
        
        def logout : Box[SessionSummaryV1] = {
            SessionManager.getSessionState(create = false) match {
                case Some(state) ⇒
                    SessionManager !! LogoutUserRequest(state, S.request) match {
                        case Full(summary : SessionSummaryV1) ⇒ Full(summary)
                        case Full(e : EmptyBox) ⇒ e
                        case _ ⇒ Failure("unexpected result from LogoutUserRequest")
                    }
                case None ⇒ Failure("no session")
            }
        }

        /**
         * Create an entry in the database for a specified web page.
         *
         * @param page the path to the page within the current application context
         * @return a `PageDef` instance for the page
         */
        def createPageDef(page : String) : PageDef = {
            val pdef = PageDef.find(By(PageDef.page, page)) openOr (new PageDef).page(page).saveMe
            thePages.synchronized {
                thePages += (page → pdef)
            }
            pdef
        }

        def logPageHit(page : String) : Box[PageDef] = {
            import java.net.URI
            tryo(new URI(page)) match {
                case Full(uri) ⇒
                  val path = uri.getPath
                  S.request flatMap { req ⇒
                      val cpath = req.contextPath
                      Log.debug("path=" + path + ", cpath=" + cpath)
                      path indexOf cpath match {
                          case i if i >= 0 ⇒
                              val j = if (path.charAt(i + cpath.length) == '/') i + 1 else i
                              val ppath = path.substring(j + cpath.length)
                              val pdef = thePages.synchronized {
                                  thePages.getOrElseUpdate(ppath, createPageDef(ppath))
                              }
                              PageHit.create
                                  .sessionId(sessionId.id)
                                  .hitTime(millis)
                                  .principal(getPrincipal.getPrincipalId)
                                  .page(pdef)
                                  .saveMe()
                              Full(pdef)

                          case _ ⇒ Empty
                      }
                  }
                case _ ⇒ Empty
            }
        }

        def getSessionFolder : SessionFolder = _sessionFolder

        def setSessionFolder(sfolder : SessionFolder) : Unit = { _sessionFolder = sfolder }

        def getSessionSummary : SessionSummaryV1 = _sessionSummary

        def setSessionSummary(summary : SessionSummaryV1) : Unit = {
            _sessionSummary = summary
        }

        def setLoggedIn(b : Boolean) : Unit = { _loggedIn_? = b }

//        def setLoginInfo(uinfo : Box[UserInfo]) {
//            _uinfo foreach (_ close ())
//            _uinfo = uinfo
//            uinfo flatMap { user ⇒
//                webSession foreach { ws ⇒
//                    val ginfo = user.getGroupInfo
//                    loginSession = LoginSession.loginUser(ws, user, ginfo)
//                    ginfo close ()
//                }
//                Full(loginSession)
//            } openOr {
//                loginSession.foreach { ls ⇒ ls.endTime(millis).save() }
//                loginSession = Empty
//                Empty
//            }
//        }
    }

    //private val filenameDateFormat = new SimpleDateFormat("yyyy-MM-dd'_'HH-mm-ss.SSSZZZ")

    /**
     * Message to request a list of all active sessions. The session ids
     * are returned as List[String].
     */
    // TODO: implement this operation
    private sealed case class SessionListRequest()
    
    /**
     * Message to request a SessionClient interface to a specified session.
     * The result is a Box[SessionClient].
     */
    private sealed case class JSessionInfoRequest(jSessionId : String)
    private sealed case class SessionInfoRequest(sessionId : ResourceId)
    
    private sealed case class LoginUserRequest(state : SessionState, req : Box[Req],
                                               username : String, password : Option[String], ginfo : GroupInfo)
    
    private sealed case class LogoutUserRequest(state : SessionState, req : Box[Req])

    private sealed case class TerminateSessionRequest(liftSession : LiftSession)

    private sealed case class MigrateSessionRequest(summary : SessionSummaryV1)
    
    /**
     * Message to request all the logins for a specified HTTP session.
     * A List[SessionClient] is returned.
     */
    sealed case class AllLoginsRequest(sid : String)
    
    /** Notification sent to listeners on session creation */
    sealed case class NotifySessionCreated(sinfo : SessionClient)
    
    /** Notification sent to listeners on session termination */
    sealed case class NotifySessionDestroyed(summary : SessionSummaryV1)
    
    sealed case class NotifyLoginUser(sinfo : SessionClient)
    
    sealed case class NotifyLogoutUser(summary : SessionSummaryV1)
    
    /** Map of active HTTP session ids to SessionFolder resource ids */
    private val jSessionToSessionId : mutable.Map[String, ResourceId] = mutable.Map()

    /** Map of SessionFolder resource ids to SessionState objects */
    private val sessionStates : mutable.Map[ResourceId, SessionState] = mutable.Map()
    
    def init() : Unit = {
        // 2015-07-06 HEP no longer automatically create a guest session for every HTTP session
        // LiftSession.afterSessionCreate = sessionCreated _ :: LiftSession.afterSessionCreate
        LiftSession.onShutdownSession = sessionDestroyed _ :: LiftSession.onShutdownSession

        // Look for any SessionFolders remaining at the top level of the archive hierarchy,
        // and attempt to move them to an archive folder for the group associated with the
        // session user.
        val farchive = getArchiveFolder
        for { seq ← farchive getMembers Some(SessionFolder.getMimeType); sfname ← seq } {
            farchive.withMember(sfname) {
                case sfolder : SessionFolder ⇒
                    archiveSession(farchive, sfolder)
                    Empty
            }
        }
        farchive close ()

        // Archive any sessions left in the active folder from the last run
        val active = getActiveFolder
        val namesBox = active getMembers Some(SessionFolder.getMimeType)
        active close ()

        namesBox foreach { sessionNames ⇒
            sessionNames foreach { sname ⇒
                active getMember sname match {
                    case Full(sfolder : SessionFolder) ⇒
                        sfolder.getData foreach { summary ⇒
                            val newsummary = SessionSummaryV1(summary.startTime, Some(millis), summary.attachTime,
                                                             summary.detachTime, summary.ipAddress, summary.sessionId,
                                                             summary.userPath, summary.userId, summary.userAgent)
                            sfolder putData newsummary
                        }
                        archiveSession (sfolder)
                        sfolder close ()
                    case Full(other) ⇒ other close ()
                    case _ : EmptyBox ⇒
                }
            }
        }
    }

    def migrateSession(summary : SessionSummaryV1) : Box[SessionFolder] = {
        SessionManager !! MigrateSessionRequest(summary) match {
            case Full(sfolder : SessionFolder) ⇒ Full(sfolder)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from MigrateSessionRequest")
        }
    }

    override def createUpdate : Any = Empty

    override def mediumPriority : PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
        override def isDefinedAt(msg : Any) : Boolean = {
            Log.info(msg.toString)
            true
        }
        override def apply(msg : Any) : Unit = {
            msg match {
                case SessionInfoRequest(sid) ⇒ doSessionInfoRequest(sid)
                case JSessionInfoRequest(jSession) ⇒ doJSessionInfoRequest(jSession)
                case LoginUserRequest(state, req, username, password, ginfo) ⇒
                    doLoginUserRequest(state, req, username, password, ginfo)
                case LogoutUserRequest(state, req) ⇒ doLogoutUserRequest(state, req)
                case TerminateSessionRequest(liftSession) ⇒ doTerminateSessionRequest(liftSession)
                case MigrateSessionRequest(summary) ⇒ doMigrateSessionRequest(summary)
                case something ⇒ Log.info("SessionManager received message: " + something.toString)
            }
        }
    }

    private def doSessionInfoRequest(sessionId : ResourceId) : Unit = {
        sessionStates.synchronized (sessionStates get sessionId) match {
            case Some(sinfo : SessionClient) ⇒ reply(sinfo)
            case _ ⇒ reply(Empty)
        }
    }

    private def doJSessionInfoRequest(jSessionId : String) : Unit = {
        jSessionToSessionId.synchronized {
            jSessionToSessionId get jSessionId flatMap { sid ⇒
                sessionStates.synchronized (sessionStates get sid)
            }
        } match {
            case Some(sinfo : SessionClient) ⇒ reply(sinfo)
            case _ ⇒ reply(Empty)
        }
    }

    private def doLoginUserRequest(state : SessionState, req : Box[Req],
                                   username : String, password : Option[String], ginfo : GroupInfo) : Unit = {
        Log.info(s"doLoginUserRequest: username=$username")
        // If the user is already logged in, log them out first
        if (state.loggedIn_?) {
            val liftSession = state.liftSession
            doLogoutUser (state, req)
            // Logout terminates the session, so make a new one
            getSessionState(liftSession, req, create = true) foreach { newState ⇒
                // The new session should not be logged in, but check anyway
                if (!newState.loggedIn_?) {
                    doLoginUserRequest(newState, req, username, password, ginfo)
                }
            }
        }
        else {
            val allowed =
            // Ensure logins are enabled for the group, or that the authenticated
            // user is a system admin.
                if (ginfo.isLoginEnabled_?) {
                    ginfo findUser_! username match {
                        case Full(uinfo) ⇒
                            // See if the user has been banned. Includes system admins.
                            val loginAllowed =
                                if (uinfo.isLoginAllowed_?) {
                                    // If the password is not defined, this must be an OAuth login, meaning
                                    // that the user has already been authenticated. If the password is
                                    // defined, that means it is not an OAuth login, which is unacceptable
                                    // if the user is marked as OAuthOnly.
                                    if (password.isDefined && uinfo.isOAuthOnly_?) {
                                        Failure("cannot authenticate this user with a password")
                                    }
                                    else Full(true)
                                }
                                else Failure("your account has been suspended")
                            uinfo close()
                            loginAllowed
                        case Empty ⇒ Failure("invalid username/password")
                        case f : Failure ⇒ f
                    }
                }
                else {
                    // Logins are disabled for the group, but a system administrator may be allowed.
                    ginfo findUser_! username match {
                        case Full(uinfo) ⇒
                            val loginAllowed =
                                if (GlobalConfig.isSystemAdmin_?(uinfo.getSelfPrincipal)) {
                                    if (uinfo.isLoginAllowed_?) Full(true)
                                    else Failure("your account has been suspended")
                                }
                                else Failure("logins are currently disabled there")
                            uinfo close()
                            loginAllowed
                        case _ : EmptyBox ⇒ Failure("logins are currently disabled there")
                    }
                }

            val result = allowed flatMap { _ ⇒
                // Validate the username and password in the login group
                ginfo validateUser(username, password) flatMap { uinfo ⇒
                    val self = uinfo.getSelfPrincipal
                    uinfo close()
                    login(state, req, self)
                }
            }
            Log.info("doLoginUserRequest: result=" + result)
            result match {
                case Full(sclient : SessionClient) ⇒
                    reply(sclient)
                    sendListenersMessage(NotifyLoginUser(sclient))
                case e : EmptyBox ⇒ reply(e)
            }
        }
    }

    def doLogoutUserRequest(state : SessionState, req : Box[Req]) : Unit = {
        reply(doLogoutUser(state, req) getOrElse Empty)
    }

    def doLogoutUser(state : SessionState, req : Box[Req]) : Option[SessionSummaryV1] = {
        val summary = state.getSessionSummary
        val (ipaddr, userAgent) = req match {
            case Full(r) ⇒ (r.remoteAddr, r.userAgent openOr summary.userAgent)
            case _ ⇒ (state.clientIpAddress, summary.userAgent)
        }
        val now = millis
        val newSummary = SessionSummaryV1(summary.startTime, Some(now), summary.attachTime, Some(now),
                                          ipaddr, summary.sessionId, summary.userPath, summary.userId,
                                          userAgent)
        val sfolder = state.getSessionFolder
        sfolder putData newSummary
        // Important to set state to logged out here, since doTerminateSession would recurse otherwise
        state.setLoggedIn(b = false)
        val summaryOpt= doTerminateSession(state.liftSession)
        sendListenersMessage(NotifyLogoutUser(newSummary))
        summaryOpt
    }

    def doTerminateSessionRequest(liftSession : LiftSession): Unit = {
        reply(doTerminateSession(liftSession) getOrElse Empty)
    }

    def doTerminateSession(liftSession : LiftSession) : Option[SessionSummaryV1] = {

        jSessionToSessionId.synchronized {
            val id = liftSession.uniqueId
            jSessionToSessionId get id flatMap { sid ⇒
                sessionStates.synchronized {
                    val ssOpt = sessionStates get sid
                    ssOpt match {
                        case Some(ss) ⇒
                            // Logging out the user will cause this function to be called again,
                            // after the user has been marked as logged out.
                            if (ss.loggedIn_?) doLogoutUser (ss, Empty)
                            else {
                                jSessionToSessionId remove id
                                sessionStates remove ss.sessionId
                                S.clearSessionRewriter
                                val summaryOpt = archiveSession(ss).toOption
                                ss.terminate()
                                summaryOpt foreach { summary ⇒
                                    sendListenersMessage(NotifySessionDestroyed(summary))
                                }
                                Log.info(s"destroyed web session id $id")
                                summaryOpt
                            }
                        case None ⇒
                            Log.info(s"Session id $id has no session state")
                            None
                    }
                }
            }
        }
    }

    def doMigrateSessionRequest(summary : SessionSummaryV1) : Unit = {
        val archive = getArchiveFolder
        val username = {
            val upath = summary.userPath
            val i = upath.lastIndexOf('/')
            if (i >= 0) upath.substring(i+1) else upath
        }
        val filename = s"$username-${summary.sessionId}"
        val options = CfsCreateOptions(ctime = summary.startTime)
        val result = archive create (filename, SessionFolder.getMimeType, options) match {
            case Full(fsession : SessionFolder) ⇒
                fsession putData summary
                // Link the session folder to a new name based on its resource id
                archive link(s"$username-${fsession.getResourceId.id}", fsession) match {
                    case Full(nfsession : SessionFolder) ⇒
                        // Unlink the session folder under the jSessionId name
                        archive unlink(filename, recursive = true)
                        fsession close()
                        Full(nfsession)
                    case Full(other) ⇒
                        other close ()
                        Failure(s"error renaming $filename")
                    case e : EmptyBox ⇒
                        Failure(s"error renaming $filename", Empty, e ?~ "(Empty)")
                }
            case Full(other) ⇒
                other close ()
                Failure("sessionCreated: SessionFolder not found where expected")
            case e : EmptyBox ⇒
                Failure(s"sessionCreated failed to create $filename", Empty, e ?~ "(Empty)")
        }
        archive close ()
        reply(result openOr result)
    }

    private def login(ss : SessionState, req : Box[Req], principal : CfsPrincipal) : Box[SessionState] = {
        val summary = ss.getSessionSummary
        val (ipaddr, userAgent) = req match {
            case Full(r) ⇒ (r.remoteAddr, r.userAgent openOr summary.userAgent)
            case _ ⇒ (ss.clientIpAddress, summary.userAgent)
        }
        val start = summary.startTime
        val newSummary = SessionSummaryV1(start, None, Some(start), None, ipaddr, summary.sessionId,
                                          principal.getPrincipalName, principal.getPrincipalId, userAgent)
        val sfolder = ss.getSessionFolder
        val afolder = getActiveFolder
        val oldname = sfolder.getName
        val newname = s"${principal.getPrincipalPath.allParts.last}-${sfolder.getResourceId.id}"
        // Make a link to the SessionFolder with the logged in username
        val result = afolder link (newname, sfolder) match {
            case Full(sfnew : SessionFolder) ⇒
                // Update the SessionFolder data
                sfnew putData newSummary
                // Remove the old name of the SessionFolder
                afolder unlink (oldname, recursive = false)
                // Update the SessionState
                ss setSessionFolder sfnew
                ss setSessionSummary newSummary
                ss setPrincipal principal
                ss setLoginGroup principal.getPrincipalPath.getParent.toString
                ss setLoggedIn true
                Full(ss)
            case Full(other) ⇒
                afolder unlink (newname, recursive = false)
                other close ()
                Failure(s"failed to rename $oldname")
            case e : EmptyBox ⇒ e
        }
        sfolder close ()
        afolder close ()
        result
    }

    /**
     * Entry point called by Lift when an HTTP request causes a new
     * session to be created.
     */
    def sessionCreated(session : LiftSession, req : Req) : Unit = {
        jSessionToSessionId.synchronized {
            val jSessionId = session.uniqueId
            // It seems sessionCreated gets called more than once for the same jSessionId.
            // Check whether we already have a session for the given jSessionId.
            val needSession = jSessionToSessionId get jSessionId match {
                case Some(sid) ⇒
                    sessionStates.synchronized {
                        sessionStates get sid
                    } match {
                        case None ⇒
                            jSessionToSessionId remove jSessionId
                            true
                        case Some(_) ⇒ false
                    }
                case None ⇒ true
            }
            Log.info(s"sessionCreated called for $jSessionId, needSession = $needSession")
            if (needSession) {
                val now = millis
                val ipaddr = req.remoteAddr
                val summary = SessionSummaryV1(now, None, Some(now), None, ipaddr, jSessionId,
                    Startup.GuestUserPath, GuestPrincipal.getPrincipalId,
                    req.userAgent openOr "")
                // The initial name for the session folder uses the jSessionId
                val filename = s"guest-$jSessionId"
                val afolder = getActiveFolder
                afolder create (filename, SessionFolder.getMimeType, CfsCreateOptions.Default) match {
                    case Full(fsession : SessionFolder) ⇒
                        fsession putData summary
                        // Link the session folder to a new name based on its resource id
                        afolder link (s"guest-${fsession.getResourceId.id}", fsession) match {
                            case Full(nfsession : SessionFolder) ⇒
                                // Unlink the session folder under the jSessionId name
                                afolder unlink (filename, recursive = true)
                                fsession close ()
                                val nfilename = nfsession.getName
                                // Make the SessionState object and add it to the tables
                                val ok = jSessionToSessionId get jSessionId match {
                                    case None ⇒ sessionStates.synchronized {
                                        sessionStates get nfsession.getResourceId match {
                                            case None ⇒
                                                val state = new SessionState(session, GuestPrincipal, nfsession, summary)
                                                sessionStates += (state.sessionId → state)
                                                jSessionToSessionId += (jSessionId → state.sessionId)
                                                Log.info(s"created web session id $jSessionId")
                                                sendListenersMessage(NotifySessionCreated(state))
                                                if (allSessionLog.get) {
                                                    LogFilter.enableLogging(jSessionId)
                                                }
                                                true
                                            case Some(_) ⇒ false    // Should not happen
                                        }
                                    }
                                    case Some(_) ⇒ false    // Should not happen
                                }
                                if (!ok) {
                                    // This should not happen
                                    Log.error(s"Session id $jSessionId already exists")
                                    afolder unlink (nfilename, recursive = true)
                                    nfsession close ()
                                }
                            case Full(other) ⇒
                                Log.error(s"error renaming $filename")
                                other close ()
                            case e : EmptyBox ⇒
                                Log.error(s"""error renaming $filename: ${(e ?~ "(Empty)").msg}""")

                        }
                    case Full(other) ⇒
                        other close ()
                        Log.error("sessionCreated: SessionFolder not found where expected")
                    case e : EmptyBox ⇒
                        Log.error(s"""sessionCreated failed to create $filename: ${(e ?~ "(Empty)").msg}""")
                }
                afolder close ()
            }
        }
    }

    /**
     * Entry point called by Lift when a session is terminated.
     */
    def sessionDestroyed(session : LiftSession) : Unit = {
        LogFilter.disableLogging(Some(session.uniqueId))
        Schedule ({ () ⇒
            if (!ShutdownListener.shutdown) {
                terminateSession (session)
            }
            else {
                Log.error("sessionDestroyed called during shutdown")
            }
        }, TimeSpan(100))
    }

    def terminateSession(liftSession : LiftSession) : Box[SessionSummaryV1] = {
        // The Lift and HTTP sessions will continue to exist, but this will terminate
        // the Choice session. The next request on the Lift will cause a new Choice
        // session to be created.
        SessionManager !! TerminateSessionRequest (liftSession) match {
            case Full(summary : SessionSummaryV1) ⇒ Full(summary)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from TerminateSessionRequest")
        }
    }

    private def archiveSession(ss : SessionState) : Box[SessionSummaryV1] = {
        val sfolder = ss.getSessionFolder
        archiveSession(sfolder)
    }

    /**
     * Archive an active session, given its SessionFolder. Closing the specified
     * SessionFolder is the responsibility of the caller.
     *
     * @param sfolder session folder for the active session
     */
    private def archiveSession(sfolder : SessionFolder) : Box[SessionSummaryV1] = {
        val factive = getActiveFolder
        val summaryBox = archiveSession(factive, sfolder)
        factive close ()
        summaryBox
    }

    /**
     * Archive a session, given its SessionFolder and the folder in which that SessionFolder
     * current resides. Archived sessions are organized into a hierarchy according to the
     * group path of the session user. The SessionFolder is linked into this hierarchy,
     * and then unlinked from its current folder. The caller is responsible for closing
     * both currentFolder and sfolder.
     *
     * @param currentFolder the folder in which the SessionFolder is currently located
     * @param sfolder the SessionFolder to be archived
     */
    def archiveSession(currentFolder : CfsFolder, sfolder : SessionFolder) : Box[SessionSummaryV1] = {
        val name = sfolder.getName
        sfolder.getDataUnchecked match {
            case Full(ssummary) ⇒
                val gpath = {
                    val upath = ssummary.userPath
                    val i = upath lastIndexOf '/'
                    if (i < 0) "" else upath substring (0, i)
                }
                CfsPath(s"${Startup.SessionFolderPath}/archived$gpath") foreach { fpath ⇒
                    val afolder = FileOps.getOrMakeFolder(fpath, SystemPrincipal, recursive = true) match {
                        case Full(gfolder) ⇒ gfolder
                        case e: EmptyBox ⇒
                            val msg = (e ?~ "Empty").messageChain
                            Log.error(s"error accessing archive folder for group $gpath: $msg")
                            getArchiveFolder
                    }
                    afolder link(name, sfolder) match {
                        case Full(sfnew) ⇒
                            sfnew close()
                            currentFolder unlink(name, recursive = false)
                        case _ ⇒
                    }
                    afolder close ()
                }
                Full(ssummary)
            case e : EmptyBox ⇒
                val msg = (e ?~ "Empty").messageChain
                Log.error(s"unable to archive session $name: $msg")
                e
        }
    }

    private def getActiveFolder : CfsFolder = getSessionSubFolder ("active")

    private def getDetachFolder : CfsFolder = getSessionSubFolder ("detached")

    def getArchiveFolder : CfsFolder = getSessionSubFolder ("archived")

    /**
     * Get or create a subfolder of the system sessions folder. Generally there are
     * subfolders for each session state: active, detached, and archived. Within these
     * subfolders, sessions are represented by SessionFolder containers.
     *
     * @param member the subfolder name
     * @return a CfsFolder representing the subfolder
     */
    private def getSessionSubFolder(member : String) : CfsFolder = {
        val sfolder = SessionFolder.getSessionsFolder(SystemPrincipal)
        val afolder = sfolder getMember member match {
            case Full(folder : CfsFolder) ⇒ folder
            case Full(other) ⇒
                other close ()
                sys.error(s"getSessionSubFolder: $member is not a folder")
            case Empty ⇒
                sfolder create (member, CfsFolder.getMimeType, CfsCreateOptions.Default) match {
                    case Full(folder : CfsFolder) ⇒ folder
                    case Full(other) ⇒
                        other close ()
                        sys.error(s"getSessionSubFolder: $member creation did not yield a folder")
                    case e : EmptyBox ⇒
                        sys.error(s"""getSessionSubFolder failed to create $member: ${(e ?~ "(Empty)").msg}""")
                }
            case f : Failure ⇒ sys.error(s"getSessionSubFolder failed to open $member: ${f.msg}")
        }
        sfolder close ()
        afolder
    }

    /**
     * Get the SessionState for the current session, if any. This probably will not work
     * from the SessionManager actor thread. It should be called before sending a message
     * to the actor.
     *
     * @return the SessionState for the current session, or None
     */
    private def getSessionState(create : Boolean) : Option[SessionState] = {
        // This reference to S.session probably resolves to a thread-private variable
        // that will not be set for the SessionManager actor thread.
        S.session match {
            case Full(liftSession) ⇒
                getSessionState(liftSession, S.request, create)
            case Empty ⇒ None
            case f : Failure ⇒
                Log.error(s"getSessionState error: ${f.msg}")
                None
        }
    }

    private def getSessionState(liftSession : LiftSession, reqBox : Box[Req],
                                create : Boolean) : Option[SessionState] = {
        jSessionToSessionId.synchronized {
            Log.info(s"getSessionState on ${liftSession.uniqueId}")
            jSessionToSessionId get liftSession.uniqueId match {
                case Some(sid) ⇒ sessionStates.synchronized(sessionStates get sid)
                case None ⇒
                    if (create) {
                        Log.info(s"getSessionState: creating session for ${liftSession.uniqueId}")
                        reqBox foreach { req ⇒ sessionCreated(liftSession, req) }
                        jSessionToSessionId get liftSession.uniqueId match {
                            case Some(sid) ⇒
                                val ssopt = sessionStates.synchronized(sessionStates get sid)
                                ssopt foreach { ss ⇒
                                    // Until the SessionState is created, the last candidate for the login group
                                    // is maintained in the LOGINGROUP HTTP session attribute. Now that the
                                    // SessionState has been created, maintain it there.
                                    val logingroup = S.session flatMap (_.httpSession) flatMap { httpSession ⇒
                                        httpSession attribute "LOGINGROUP" match {
                                            case s : String ⇒ Full(s)
                                            case _ ⇒ Empty
                                        }
                                    } openOr Startup.DefaultUsersPath
                                    ss setLoginGroup logingroup
                                }
                                ssopt
                            case None ⇒
                                Log.error(s"getSessionState: automatic session creation failed for ${liftSession.uniqueId}")
                                None
                        }
                    }
                    else None
            }
        }
    }

    /**
     * Get the SessionClient interface for the current session.
     *
     * @return a SessionClient or None
     */
    def getSessionClient(create : Boolean = true) : Option[SessionClient] = getSessionState(create = create)
}
