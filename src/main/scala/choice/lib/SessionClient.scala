/**
  * Copyright © 2013-2017 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.lib

import net.liftweb.http.{LiftResponse, LiftSession}
import net.liftweb.common._
import choice.access.{CfsPrincipal, Principal, SystemPrincipal}
import choice.fs.{GroupInfo, SessionFolder, SessionSummaryV1, UserInfo}
import choice.model.{PageDef, ResourceId}
import choice.actor.SessionManager

/**
 * Define the interface to session information returned to a client or listener
 * of the SessionManager. This also includes operations that change the state
 * of the session, which are generally implemented in the SessionManager itself.
 */
trait SessionClient extends SessionActivityMap {

    /** The HTTP session id of this session */
    def jSessionId : String

    /** The associated Lift session object */
    def liftSession : LiftSession

    /** The client IP address */
    def clientIpAddress : String

    /** The client browser's user agent string */
    def userAgent : String

//    /**
//     * Clock skew between client and server. Add this to client times to
//     * get server times.
//     */
//    def clockSkew : Long
//
//    def setClientTime(tstamp : Long) : Box[SessionClient]

    def sessionId : ResourceId

    def getPrincipal : CfsPrincipal

    /**
     * Get the pathname of the login group, initially the default user group.
     * The login group is only used when the user logs in, as the group assumed
     * to contain the username given for the login. It is ignored if the login
     * user is specified as a full path.
     *
     * @return the login group file path
     */
    def getLoginGroup : String

    /**
     * Set the login group.
     *
     * @param gpath the group file path
     * @return true if the group exists. Otherwise false and the login group is set
     *         to the default user group.
     */
    def setLoginGroup(gpath : String) : Boolean

    /** Is the user logged in? */
    def loggedIn_? : Boolean

    /**
      * Get the type of login. This will be "normal" unless an external account,
      * such as Google or Facebook is used, in which case it would be "google"
      * or "facebook", respectively.
      *
      * @return the login type, or "none" if the user is not logged in
      */
    def getLoginType : String

    /**
      * Set the type of login. This should only be necessary if an external account,
      * such as Google or Facebook is used.
      *
      * @param loginType the login type, e.g. "google" or "facebook"
      */
    def setLoginType(loginType : String) : Unit

    /** Get UserInfo for the current user, possibly 'guest' */
    def getUserInfo(principal : Principal) : Box[UserInfo]

    /**
     * Get GroupInfo for the group containing the current user. This could be
     * a descendant group of the login group, or the same as the login group.
     * This does not require the user to be logged in.
     *
     * @return a boxed GroupInfo
     */
    def getGroupInfo(principal : Principal) : Box[GroupInfo]

    def login(username : String, password : Option[String], ginfo : GroupInfo) : Box[SessionClient]

    def logout : Box[SessionSummaryV1]

    def onLogout(f : SessionClient ⇒ Unit) : Unit = {}

    def terminate() : Unit

    def logPageHit(page : String) : Box[PageDef]

    def getSessionFolder : SessionFolder
}

object SessionClient {

    def withCurrentUser[T](f : (UserInfo, GroupInfo) ⇒ Box[T]) : Box[T] = {
        SessionManager.getSessionClient(create = true) match {
            case Some(sclient) ⇒
                sclient.getUserInfo(SystemPrincipal) flatMap { uinfo ⇒
                    val uresult = sclient.getGroupInfo(SystemPrincipal) flatMap { ginfo ⇒
                        val gresult = f (uinfo, ginfo)
                        ginfo close ()
                        gresult
                    }
                    uinfo close ()
                    uresult
                }
            case None ⇒ Failure("no session")
        }
    }

    def requireLogin(f : (UserInfo, GroupInfo) ⇒ Box[LiftResponse]) : Box[LiftResponse] = {
        SessionManager.getSessionClient(create = true) match {
            case Some(sclient) if sclient.loggedIn_? ⇒
                sclient.getUserInfo(SystemPrincipal) flatMap { uinfo ⇒
                    val uresult = sclient.getGroupInfo(SystemPrincipal) flatMap { ginfo ⇒
                        val gresult = f (uinfo, ginfo)
                        ginfo close ()
                        gresult
                    }
                    uinfo close ()
                    uresult
                }
            case Some(_) ⇒ Failure("not logged in")
            case None ⇒ Failure("no session")
        }
    }

    def withSessionState[T](requireLogin : Boolean = true)(f : SessionClient ⇒ Box[T]) : Box[T] = {
        SessionManager.getSessionClient(create = true) match {
            case Some(sclient) ⇒
                if (requireLogin) {
                    if (sclient.loggedIn_?) f(sclient)
                    else Failure("not logged in")
                }
                else f(sclient)
            case None ⇒ Failure("session not found")
        }
    }
}