/**
  * Copyright © 2011-2017 The Board of Trustees of The Leland Stanford Junior University.
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

import choice.access.Principal
import choice.fs._
import choice.lib.JsonHelpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._

import scala.annotation.tailrec

/**
 * Group operations invoked through Ajax requests.
 *
 * @author Howard Palmer
 *
 */
object GroupOps {

    implicit val formats : DefaultFormats.type = DefaultFormats

    object Log extends Logger
    
    def getGroups(ginfo : GroupInfo, all : Boolean) : Box[Map[String, Any]] = {
        val groups = if (all) ginfo.getGroups_! else ginfo.getGroups
        val list = groups
            .map(_.asMap)
        val respmap = Map("status" → 1, "groups" → list)
        Full(respmap)
    }

    def getUsers(ginfo : GroupInfo, all : Boolean) : Box[Map[String, Any]] = {
        val users = (if (all) ginfo.getUsers_! else ginfo.getUsers)
            .toArray
            .sortBy(_.getUsername)
            .map(_.asMap)
        val respmap : Map[String, Any] = Map(
            "status" → 1,
            "users" → users)
        Full(respmap)
    }

    sealed case class AjaxCreateGroup(group : String, desc : Option[String])
        extends AjaxApiRequest("group", "create") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val resultMap =
                Cfs.withValidPath(group) { gpath ⇒
                    Cfs.ifNoFile(gpath, self, CfsOpenOptions.Default) { gp ⇒
                        val parent = gp.getParent
                        def doCreate(folder : CfsFolder) : Box[Map[String, Any]] = {
                            val gdesc = GroupDesc(None, desc, None, None, None, None, None, None, None, None, None)
                            val options = GroupCreateOptions(gdesc = Some(gdesc))
                            folder create (gp.getFileName.toString, GroupInfo.getMimeType, options) match {
                                case Full(tginfo : GroupInfo) ⇒
                                    val map = tginfo.asMap
                                    tginfo close ()
                                    Full(map)
                                case Full(vfile) ⇒
                                    vfile close ()
                                    Failure(s"unexpected result from creating group ${gp.toString}")
                                case e : EmptyBox ⇒ e
                            }
                        }
                        Cfs.withExistingFile (parent, self) {
                            case pginfo : GroupInfo ⇒ doCreate(pginfo)
                            case folder : CfsFolder ⇒ GroupInfo.canCreateGroup (folder) { () ⇒
                                doCreate(folder)
                            }
                        }
                    }
                }
            resultMap flatMap MapResponse
        }
    }

    sealed case class AjaxGroupList(group : String, recursive : Option[Boolean])
        extends AjaxApiRequest("group", "groups") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile (group, self, CfsOpenOptions.Default) {
                    case tginfo : GroupInfo ⇒
                        val glist =
                            if (recursive getOrElse false) tginfo.getGroups_!
                            else tginfo.getGroups
                        val gmaps = glist map { g ⇒
                            val map = g.asMap
                            g close ()
                            map
                        }
                        Full(gmaps)
                } flatMap (groupmaps ⇒ MapResponse(Map("groups" → groupmaps)))
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxUsersList(group : String, recursive : Option[Boolean])
        extends AjaxApiRequest("group", "users") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile (group, self, CfsOpenOptions.Default) {
                    case tginfo : GroupInfo ⇒
                        val ulist =
                            if (recursive getOrElse false) tginfo.getUsers_!
                            else tginfo.getUsers
                        val umaps = ulist map { u ⇒
                            val map = u.asMap
                            u close ()
                            map
                        }
                        Full(umaps)
                } flatMap (usermaps ⇒ MapResponse(Map("users" → usermaps)))
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxAddUsers(group : String, users : List[String]) extends AjaxApiRequest("group", "addusers") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                // For usernames not qualified with a group path, the default source group is
                // the current user's group.
                val defaultSourceGroup = sclient getGroupInfo self match {
                    case Full(sginfo) ⇒
                        val path = sginfo.getPath.toString
                        sginfo close ()
                        path
                    case _ : EmptyBox ⇒ ""
                }
                // Look up the target group
                val tumaps : Box[List[Map[String, Any]]] = Cfs.withExistingFile (group, self, CfsOpenOptions.Default) {
                    case tginfo : GroupInfo ⇒
                        val maplist : List[Map[String, Any]] = this.users.map { userstring ⇒
                            val (ugpath, newuname) = {
                                val i = userstring.lastIndexOf(":")
                                if (i < 0) (userstring, None)
                                else (userstring.substring(0, i), Some(userstring.substring(i + 1).toLowerCase))
                            }
                            val (gpath, username) = {
                                val j = ugpath.lastIndexOf("/")
                                // If username is unqualified with a group, use the current user's group
                                if (j < 0) (defaultSourceGroup, ugpath)
                                else (ugpath.substring(0, j), ugpath.substring(j + 1))
                            }
                            val lcusername = username.toLowerCase
                            val rename = newuname.map(nn ⇒ Map(lcusername → nn))
                            val mapbox = Cfs.withExistingFile (gpath, self, CfsOpenOptions.Default) {
                                case sginfo : GroupInfo ⇒ Full(sginfo)
                                    val map = sginfo copyUsers (tginfo, List(lcusername), rename) match {
                                        case (_, Full(tuinfo)) :: Nil ⇒
                                            val tumap = tuinfo.asMap
                                            tuinfo close ()
                                            tumap + ("status" → 1)
                                        case (tuname, Empty) :: Nil ⇒ Map[String, Any]("status" → -1,
                                                                                       "user" → tuname,
                                                                                       "msg" → "(Empty)")
                                        case (tuname, f : Failure) :: Nil ⇒
                                            Map[String, Any]("status" → -1, "user" → tuname, "msg" → f.messageChain)
                                        case _ ⇒ Map[String, Any]("status" → -1,
                                                                  "user" → lcusername,
                                                                  "msg" → "unexpected result from copyUsers")
                                    }
                                    Full(map)
                            }
                            mapbox match {
                                case Full(map) ⇒ map
                                case e : EmptyBox ⇒
                                    Map[String, Any]("status" → -1,
                                                     "user" → username, "msg" → (e ?~ "(Empty)").messageChain)
                            }
                        }
                        Full(maplist)
                }
                tumaps flatMap ArrayResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxRemUsers(group : String, users : List[String]) extends AjaxApiRequest("group", "remusers") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val maplistbox : Box[List[Map[String, Any]]] =
                    Cfs.withExistingFile (group, self, CfsOpenOptions.Default) {
                        case tginfo : GroupInfo ⇒
                            val list = tginfo removeUsers (users map (_.toLowerCase))
                            val maplist = list map { pair ⇒
                                val (tguname, rstatus) = pair
                                rstatus match {
                                    case Full(b) ⇒ Map[String, Any]("status" → 1, "user" → tguname, "result" → b)
                                    case Empty ⇒ Map[String, Any]("status" → -1, "user" → tguname, "msg" → "(Empty)")
                                    case f : Failure ⇒
                                        Map[String, Any]("status" → -1, "user" → tguname, "msg" → f.msg)
                                }
                            }
                            Full(maplist)
                }
                maplistbox flatMap ArrayResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxRemGroup(group : String, recursive : Option[Boolean])
        extends AjaxApiRequest("group", "remgroup") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(group) { gpath ⇒
                    val groupName = gpath.allParts.last
                    val parent = gpath.getParent
                    Cfs.withExistingFile(parent, self, CfsOpenOptions.Default) {
                        case folder : CfsDirFile ⇒
                            folder.withMember(groupName) {
                                case tginfo : GroupInfo ⇒
                                    folder unlink (groupName, recursive getOrElse false)
                            }
                    }
                } flatMap (_ ⇒ SuccessResponse)
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxCopyUsers(dstgroup : String, srcgroup : String, users : List[String],
                                    rename : Option[Map[String, String]])
        extends AjaxApiRequest("group", "copyusers") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                // Lookup source group info
                Cfs.withExistingFile (srcgroup, self, CfsOpenOptions.Default) {
                    case sginfo : GroupInfo ⇒
                        // Lookup target group info
                        Cfs.withExistingFile (dstgroup, self, CfsOpenOptions.Default) {
                            case tginfo : GroupInfo ⇒
                                val lcusers = users map (_.toLowerCase)
                                val lcrename = rename map { rmap ⇒
                                    rmap map { pair ⇒ (pair._1.toLowerCase, pair._2.toLowerCase) }
                                }
                                val ulist = sginfo copyUsers (tginfo, lcusers, lcrename)
                                val maplist = ulist map { pair ⇒
                                    val (tuname, ubox) = pair
                                    val map : Map[String, Any] = ubox match {
                                        case Full(tuinfo) ⇒
                                            val result = Map("status" → 1,
                                                "user" → tuname,
                                                "info" → tuinfo.asMap)
                                            tuinfo close ()
                                            result
                                        case Empty ⇒ Map("status" → -1, "user" → tuname, "msg" → "(Empty)")
                                        case f : Failure ⇒ Map("status" → -1, "user" → tuname, "msg" → f.msg)
                                    }
                                    map
                                }
                                Full(maplist)
                        }
                } flatMap ArrayResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxMoveGroup(dstpath : String, srcgroup : String) extends AjaxApiRequest("group", "move") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile (srcgroup, self, CfsOpenOptions.Default) {
                case gsrc : GroupInfo ⇒
                    val srcname = gsrc.getName
                    // If a container given by the full dstpath exists, we assume the srcgroup will be
                    // added to it, maintaining its current name. Otherwise we look for a container
                    // given by the dstpath minus its last component. If that is found, the srcgroup
                    // will be added to it with the name given by the last component of dstpath.
                    val dbox = Cfs.withValidPath(dstpath) { dpath ⇒
                        Cfs open (dpath, self, CfsOpenOptions.Default) match {
                            case Full(dstgroup : GroupInfo) ⇒ Full((dstgroup, srcname))
                            case Full(folder : CfsFolder) ⇒ Full((folder, srcname))
                            case Full(cfsfile) ⇒
                                cfsfile close ()
                                Failure(s"$dstpath is not a folder or a group")
                            case Empty ⇒
                                val dstname = dpath.getFileName.toString
                                Cfs open (dpath.getParent, self, CfsOpenOptions.Default) match {
                                    case Full(dstgroup : GroupInfo) ⇒ Full((dstgroup, dstname))
                                    case Full(folder : CfsFolder) ⇒ Full((folder, dstname))
                                    case Full(cfsfile) ⇒
                                        val fname = cfsfile.getPath.toString
                                        cfsfile close ()
                                        Failure(s"$fname is not a folder or group")
                                    case Empty ⇒ Failure("destination does not exist")
                                    case f : Failure ⇒ f
                                }
                            case f : Failure ⇒ f
                        }
                    }
                    dbox flatMap { pair ⇒
                        val (dstfolder, dstname) = pair
                        val result = dstfolder link (dstname, gsrc) match {
                            case Full(dstnew) ⇒
                                dstnew close ()
                                Cfs.withExistingFile (gsrc.getPath.getParent, self) {
                                    case srcfolder : CfsFolder ⇒
                                        srcfolder unlink (srcname, true) match {
                                            case Full(_) ⇒ SuccessResponse
                                            case e : EmptyBox ⇒ FailureResponse(e)
                                        }
                                }
                            case e : EmptyBox ⇒ FailureResponse(e)
                        }
                        dstfolder close ()
                        result
                    }
            }
        }
    }

    case class GroupAttrValue(name : String, value : JValue) {
        def getBooleanValue : Box[Boolean] = {
            value match {
                case JBool(b) ⇒ Full(b)
                case JInt(i) ⇒ Full(i.toInt != 0)
                case JString(s) ⇒ s match {
                    case "on" | "yes" ⇒ Full(true)
                    case "off" | "no" ⇒ Full(false)
                    case _ ⇒ Failure(s"invalid value $s for boolean attribute $name")
                }
                case _ ⇒ Failure(s"invalid type for value of boolean attribute $name")
            }
        }
        def getIntValue : Box[BigInt] = {
            value match {
                case JInt(i) ⇒ Full(i)
                case JNull | JNothing ⇒ Empty
                case _ ⇒ Failure(s"invalid type for value of integer attribute $name")
            }
        }
        def getStringValue : Box[String] = {
            value match {
                case JString(s) ⇒ Full(s)
                case JNull | JNothing ⇒ Empty
                case _ ⇒ Failure(s"invalid type for value of string attribute $name")
            }
        }
    }

    sealed case class AjaxGroupAttr(group : String, attrs: List[GroupAttrValue])
        extends AjaxApiRequest("group", "gattr") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile (group, self, CfsOpenOptions.Default) {
                case tginfo : GroupInfo ⇒
                    tginfo.getData flatMap { gdesc ⇒
                        val flist = attrs map { attr ⇒
                            attr.name match {
                                case "desc" | "description" ⇒
                                    attr.getStringValue match {
                                        case Full(s) ⇒ Full((desc : GroupDesc) ⇒ desc setDescription Some(s))
                                        case Empty ⇒ Full((desc : GroupDesc) ⇒ desc setDescription None)
                                        case f : Failure ⇒ f
                                    }
                                case "login" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setLoginEnabled b }
                                case "signup" | "reg" | "register" | "registration" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setSignupEnabled b }
                                case "captcha" | "recaptcha" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setCaptchaEnabled b }
                                case "home" ⇒
                                    attr.getStringValue match {
                                        case Full(s) ⇒ Full((desc : GroupDesc) ⇒ desc setHomePage Some(s))
                                        case Empty ⇒ Full((desc : GroupDesc) ⇒ desc setHomePage None)
                                        case f : Failure ⇒ f
                                    }
                                case "guest" ⇒
                                    attr.getStringValue match {
                                        case Full(s) ⇒ Full((desc : GroupDesc) ⇒ desc setGuestPage Some(s))
                                        case Empty ⇒ Full((desc : GroupDesc) ⇒ desc setGuestPage None)
                                        case f : Failure ⇒ f
                                    }
                                case "regKey" ⇒
                                    attr.getStringValue match {
                                        case f : Failure ⇒
                                            attr.getIntValue match {
                                                case Full(i) ⇒ Full((desc : GroupDesc) ⇒ desc setRegKey Some(i.toString))
                                                case _ ⇒ f
                                            }
                                        case s ⇒ Full((desc : GroupDesc) ⇒ desc setRegKey s)
                                    }
                                case "verifyEmailEnabled" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setVerifyEmailEnabled b }
                                case "googleLoginEnabled" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setGoogleLoginEnabled b }
                                case "googleSignupEnabled" ⇒
                                    attr.getBooleanValue map { b ⇒ (desc : GroupDesc) ⇒ desc setGoogleSignupEnabled b }
                                case _ ⇒ Failure(s"unknown group attribute $attr")
                            }
                        }
                        val errors = (flist foldLeft (Nil : List[String])) { (list, fbox) ⇒
                            fbox match {
                                case f : Failure ⇒ f.msg :: list
                                case _ ⇒ list
                            }
                        }
                        errors match {
                            case Nil ⇒
                                val newgdesc = (flist foldLeft gdesc) { (ngdesc, fbox) ⇒
                                    fbox map { f ⇒ f(ngdesc) } openOr ngdesc
                                }
                                if (!(newgdesc eq gdesc)) {
                                    tginfo putData newgdesc flatMap { _ ⇒
                                        MapResponse(tginfo.asMap)
                                    }
                                }
                                else MapResponse(tginfo.asMap)
                            case _ ⇒ SimpleResponse(-1, errors.mkString("\n"))
                        }
                    }
            }
        }
    }

    sealed case class DistElem(group : String, weight : Int)

    def assignSubgroup(uinfo : UserInfo, dist : Option[List[DistElem]]) : Box[Map[String, Any]] = {
        val ginfo = uinfo.getGroupInfo
        val username = uinfo.getUsername
        val subgroups = ginfo.getGroups
        val respbox = subgroups find { sginfo ⇒
            sginfo.withMember (username) { case _ : UserInfo ⇒ Full(true) } openOr false
        } match {
            case Some(sginfo) ⇒ Failure(s"already assigned to ${sginfo.getName}")
            case None ⇒
                @tailrec
                def distHelper(inlist : List[DistElem],
                               acc : List[(GroupInfo, Int)]) : Box[List[(GroupInfo, Int)]] = {
                    inlist match {
                        case Nil ⇒ Full(acc)
                        case head :: tail ⇒
                            subgroups find (_.getName == head.group) match {
                                case Some(g) ⇒ distHelper(tail, (g, head.weight) :: acc)
                                case None ⇒ Failure(head.group + " does not exist")
                            }
                    }
                }
                // Get the subgroups named in dist with their weights, or if no dist,
                // get all subgroups with a weight of 1.
                val weights = dist match {
                    case Some(d) ⇒ distHelper(d, Nil)
                    case None ⇒ Full(subgroups map ((_, 1)))
                }
                weights flatMap { wlist ⇒
                    // Get the sum of the weights in the desired distribution
                    val sum = (wlist foldLeft 0.0) { (total, pair) ⇒
                        val (_, w) = pair
                        total + w
                    }
                    if (sum <= 0.0) Failure(s"invalid sum of weights: $sum")
                    else {
                        // Express the distribution weights as fractions of the weight total
                        val distgf = wlist map {
                            case (mgroup, w) ⇒ (mgroup, w/sum)
                        }
                        // Get counts of users in each subgroup
                        val usercounts = distgf map {
                            case (mgroup, f) ⇒
                                val users = mgroup.getUsers
                                val count = users.length.toLong
                                users foreach (_ close ())
                                (mgroup, f, count)
                        }
                        // Get the total number of users in all the subgroups
                        val totalusers = (usercounts foldLeft 0.0) { (total, triple) ⇒
                            val (_, _, count) = triple
                            total + count
                        }
                        @tailrec
                        def findLowGroup(list : List[(GroupInfo, Double, Long)],
                                         worst : Double, group : Box[GroupInfo]) : Box[GroupInfo] = {
                            list match {
                                case Nil ⇒ group
                                case head :: tail ⇒
                                    val (mgroup, fdesired, ucount) = head
                                    val factual = if (totalusers == 0.0) 0.0 else ucount / totalusers
                                    val deficit = fdesired - factual
                                    if (deficit > worst) findLowGroup(tail, deficit, Full(mgroup))
                                    else findLowGroup(tail, worst, group)
                            }
                        }
                        // Find the subgroup most in need of users
                        findLowGroup(usercounts, -2.0, Failure("no subgroups present")) flatMap { sginfo ⇒
                            sginfo addUser (username, uinfo) flatMap { suinfo ⇒
                                val mapresp = Map[String, Any]("status" → 1,
                                                               "username" → username,
                                                               "group" → sginfo.getName,
                                                               "gid" → sginfo.getResourceId.id)
                                suinfo close ()
                                Full(mapresp)
                            }
                        }
                    }
                }
        }
        subgroups foreach (_ close ())
        respbox
    }

    /**
     * Assign the current user to a subgroup of their current group, based on a distribution
     * which assigns weights to subgroups. If a distribution is not specified, all subgroups
     * are equally weighted.
     *
     * @param dist an optional list of subgroup names and weights
     */
    sealed case class AjaxAssignSubgroup(dist : Option[List[DistElem]]) extends AjaxApiRequest("group", "asub") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            sclient getUserInfo self flatMap { uinfo ⇒
                val result = assignSubgroup(uinfo, dist)
                uinfo close ()
                result
            } flatMap MapResponse
        }
    }

    def init() : Unit = {
        LiftRules.dispatch.prepend {
            // register the group operations handler
            case r @ Req(_, _, PostRequest) if r.param("api") == Full("group") ⇒ () ⇒ handleOp(r)
        }
    }

    //lazy val root = UserGroup.getRoot

    val optable : Map[String, AjaxApiRequestExtractor[AjaxRequest]] = Map (
        "groups" → AjaxApiRequestExtractor[AjaxGroupList]("group", "groups"),
        "users" → AjaxApiRequestExtractor[AjaxUsersList]("group", "users"),
        "create" → AjaxApiRequestExtractor[AjaxCreateGroup]("group", "create"),
        "addusers" → AjaxApiRequestExtractor[AjaxAddUsers]("group", "addusers"),
        "remusers" → AjaxApiRequestExtractor[AjaxRemUsers]("group", "remusers"),
        "copyusers" → AjaxApiRequestExtractor[AjaxCopyUsers]("group", "copyusers"),
        "remgroup" → AjaxApiRequestExtractor[AjaxRemGroup]("group", "remgroup"),
        "move" → AjaxApiRequestExtractor[AjaxMoveGroup]("group", "move"),
        "gattr" → AjaxApiRequestExtractor[AjaxGroupAttr]("group", "gattr")
    )

    val requestExtractor = new AjaxRequestTable(optable)

    def handleOp(req : Req) : Box[LiftResponse] = {
        requestExtractor.getReq(req) match {
            case Full(ajax) ⇒ ajax processRequest req
            case f : Failure ⇒ FailureResponse(f)
            case Empty ⇒ SimpleResponse(-1, "missing user operation")
        }
    }
}
