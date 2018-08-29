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
  * Web interface for event log operations.
  *
  * @author Howard Palmer
  */
package choice.lib

import choice.access.{Principal, SystemPrincipal}
import choice.fs._
import choice.lib.EventOps.systemChoiceletLib
import choice.lib.JsonHelpers._
import choice.model.ResourceId
import choice.script._
import net.liftweb.common._
import net.liftweb.http.LiftRules.DispatchPF
import net.liftweb.http._
import net.liftweb.json._

// Old style Choicelet event
// case class ChoiceletEvent(tstamp : Long, model : String, action : String, args : JArray)

/**
  * This is a specialization of the SessionLogger SessionActivity, which is used to log
  * events from a Choicelet. The log file in this case is named "Events", and resides in
  * a subfolder of the Choicelet's home folder.
  *
  * @param choicelet the Choicelet definition
  * @param stpath path to the Choicelet state folder
  * @param chstate initial Choicelet state
  * @param eventsWriter a writer for logged Choicelet events
 */
class ChoiceletLogger(val choicelet : ChoiceletDefinition,
                      stpath : CfsAbsolutePath, chstate : ChoiceletState,
                      eventsWriter : CfsBufferedWriter) extends SessionLogger(eventsWriter) {
    implicit val formats : DefaultFormats.type = DefaultFormats

    def getChoiceletHome : Box[ChoiceletHomeHandle] = Box(EventOps.systemChoiceletLib.getChoiceletHome(getChoiceletId))

    def getModelName : String = choicelet.name
    def getModelVersion : String = choicelet.version
    def getChoiceletId : Long = choicelet.id
    def getStateId : Long = chstate.id
    def getStatePath : CfsAbsolutePath = stpath
    def getInitialState : ChoiceletState = chstate

    def logEvent(event : JValue) : Box[Boolean] = {
        val eventstr = compactRender(event) + ",\n"
        val result = this put eventstr
        this flush ()
        result
    }

    def closeState() : Box[ChoiceletState] = {
        systemChoiceletLib.closeChoiceletState(getStateId)
    }
}

object ChoiceletLogger {
    def create(sclient : SessionClient, modelName : String, modelVersion : String,
               startClient : Long, args : JValue) : Box[ChoiceletLogger] = {
        // Get the session folder of the current session. No need to close it, as the SessionClient
        // owns it.
        val sfolder = sclient.getSessionFolder
        create(sfolder, modelName, modelVersion, startClient, args)
    }

    def create(sfolder : SessionFolder, modelName : String, modelVersion : String,
               startClient : Long, args : JValue) : Box[ChoiceletLogger] = {
        systemChoiceletLib.findOrCreateChoiceletHome(modelName, modelVersion) match {
            case Full(chhome) ⇒
                try {
                    val modelName = chhome.name
                    val modelVersion = chhome.version
                    chhome.createChoiceletState(sfolder, startClient, compactRender(args)) flatMap {
                        case (stpath, chstate) ⇒
                            val choicelet : ChoiceletDefinition = new ChoiceletDefinition {
                                override val id : Long = chstate.client
                                override val name : String = modelName
                                override val version : String = modelVersion
                            }

                            systemChoiceletLib.newEventsWriter(stpath) map { eventsWriter ⇒
                                new ChoiceletLogger(choicelet, stpath, chstate, eventsWriter)
                            }
                    }
                }
                finally {
                    chhome close ()
                }
            case e : EmptyBox ⇒
                e ?~! s"failed to create home folder for Choicelet $modelName version $modelVersion"
        }
    }
}

/**
 * Handle Ajax requests related to GUI event logging.
  *
  * @author Howard Palmer
 *
 */
object EventOps {
    implicit val formats : DefaultFormats.type = DefaultFormats

    private val Log = Logger("choice.lib.EventOps")

    val systemChoiceletLib : ChoiceletLib = ChoiceletLib(SystemPrincipal)

    def init() : Unit = {
        val rules : DispatchPF = {
            // register the user operations handler
            case r @ Req(_, _, PostRequest) if r.param("api") == Full("event") ⇒ () ⇒ handleOp(r)
        }
        LiftRules.dispatch.prepend(rules)

        // Scan Choicelet logs for ones that do not appear to be linked under their associated
        // session in /System/Sessions. Try to make a link to the Choicelet log from the
        // SessionFolder for any that are found.

//        systemChoiceletLib.withChoiceletFolder { choicelet ⇒
//            // For each folder corresponding to a Choicelet name
//            for (chfolder ← choicelet.getFolders) {
//                val chname = chfolder.getName
//
//                // For each folder corresponding to a version of that Choicelet
//                for (chhome ← systemChoiceletLib.getChoicelets(ChoiceletSpec(None, Some(chname), None, None))) {
//                    // For each folder corresponding to a recorded session for that Choicelet version
//                    for (chstate ← chhome.getChoiceletStateStream(None)) {
//                        systemChoiceletLib.ensureSessionFolderLink(chstate.id)
//                    }
//                    chhome close ()
//                }
//                chfolder close ()
//            }
//            Empty
//        }
    }

    def makeStateMap(choicelet : ChoiceletDefinition,
                     stpath : CfsAbsolutePath, chstate : ChoiceletState) : Map[String, Any] = {
        val evval = systemChoiceletLib.readEvents(stpath) openOr Nil
        Map("status" → 1,
            "state" → chstate.id,
            "start" → chstate.stime,
            "tstamp" → chstate.tstamp,
            "client" → choicelet.asMap,
            "args" → parseOpt(chstate.json),
            "events" → evval)
    }

   sealed case class AjaxFindClients(query : ChoiceletSpec) extends AjaxApiRequest("event", "findClients") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // The query argument must be present, even if it is empty
            if (req.json map (_ \ "query" == JNothing) openOr true) {
                Failure("missing query in event request")
            }
            else if (sclient.loggedIn_?) {
                val choiceletLib = ChoiceletLib(self)
                val clients = choiceletLib.getChoicelets(query) map { chhome ⇒
                    val result = chhome.asMap
                    chhome close ()
                    result
                }
                MapResponse(Map("status" → clients.length, "clients" → clients))
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxClientSummary(query : ChoiceletSummarySpec) extends AjaxApiRequest("event", "clientSummary") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // The query argument must be present, even if it is empty
            if (req.json map (_ \ "query" == JNothing) openOr true) {
                Failure("missing query in event request")
            }
            else if (sclient.loggedIn_?) {
                val choiceletLib = ChoiceletLib(self)
                val clients = choiceletLib.getChoiceletSummary(query)
                MapResponse(Map("status" → clients.length, "clients" → clients))
            }
            else Failure("not logged in")
        }
    }


    sealed case class AjaxStateSummary(query : ChoiceletSummarySpec) extends AjaxApiRequest("event", "stateSummary") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // The query argument must be present, even if it is empty
            if (req.json map (_ \ "query" == JNothing) openOr true) {
                Failure("missing query in event request")
            }
            else if (sclient.loggedIn_?) {
                val choiceletLib = ChoiceletLib(self)
                val clientMap = choiceletLib.getStateSummary(query)
                MapResponse(Map("status" → clientMap.length, "clients" → clientMap))
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxFindState(query : ChoiceletSummarySpec) extends AjaxApiRequest("event", "findState") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // The query argument must be present, even if it is empty
            if (req.json map (_ \ "query" == JNothing) openOr true) {
                Failure("missing query in event request")
            }
            else if (sclient.loggedIn_?) {
                val choiceletLib = ChoiceletLib(self)
                val mapArrayBox = choiceletLib.findState(query)
                mapArrayBox flatMap { array : Array[Map[String, Any]] ⇒
                    MapResponse(Map("status" → array.length, "state" → array))
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxLoadState(client : Long, id : Long) extends AjaxApiRequest("event", "loadState") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val choiceletLib = ChoiceletLib(self)
                choiceletLib.getExistingStateInfo(id) match {
                    case Full((stpath, chstate)) ⇒
                        if (chstate.client == client) {
                            choiceletLib.getChoiceletHome(client) flatMap { chhome ⇒
                                try {
                                    MapResponse(makeStateMap(chhome, stpath, chstate))
                                }
                                finally {
                                    chhome close ()
                                }
                            }
                        }
                        else Failure(s"Choicelet id $client has no state with id $id")
                    case Empty ⇒ SimpleResponse(-1, "no such state")
                    case f : Failure ⇒ f
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxGetState(client : ChoiceletSpec) extends AjaxApiRequest("event", "getState") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // The typical case is that there's a guievent activity for this session, and
            // it's associated with the Choicelet specified in the ClientSpec. Any
            // other guievent activity is closed.
            val chstatebox = sclient.withActivity ("guievent") {
                case chlogger : ChoiceletLogger  ⇒
                    if (client isSameChoicelet_? chlogger.choicelet) {
                        Full((chlogger.choicelet, chlogger.getStatePath, chlogger.getInitialState))
                    }
                    else {
                        // There is an active Choicelet log, but it's not the one we want
                        sclient closeActivity "guievent"
                        Empty
                    }
            }
//            // If there wasn't a suitable guievent activity, the next step is to search for
//            // the most recent, open Choicelet state for the Choicelet identified by the
//            // ClientSpec, and for the current session.
//            val boxchstate = chstatebox or {
//                client.getChoiceletHome flatMap { chhome ⇒
//                    val result = mostRecentOpenState(sclient.sessionId.id,
//                                                     getChoiceletStateStream (chhome.chhome),
//                                                     Empty) map { sth ⇒
//                        // Set up the most recent open state for the Choicelet and this session
//                        // as the guievent logger for this session
//                        val options = List(
//                            StandardOpenOption.WRITE, StandardOpenOption.APPEND, StandardOpenOption.CREATE,
//                            MIME_TYPE("application/json")
//                        )
//                        implicit val principalImpl = () ⇒ SystemPrincipal
//                        tryo(CfsFiles.newBufferedWriter(sth.chstate.getPath / "Events", options : _*)) foreach { out ⇒
//                            val eventlog = new ChoiceletLogger(chhome.cindex.id, sth.stindex.id, out)
//                            sclient putActivity("guievent", eventlog)
//                        }
//                        sth
//                    }
//                    chhome close ()
//                    result
//                }
//            }
//            boxchstate match {
            chstatebox match {
                case Full((choicelet, stpath, chstate)) ⇒ MapResponse(makeStateMap(choicelet, stpath, chstate))
                case Empty ⇒ NilResponse
                case f : Failure ⇒ FailureResponse(f)
            }
        }

//        def mostRecentOpenState(sid : Long, s : Stream[ChoiceletStateHolder],
//                                found : Box[ChoiceletStateHolder]) : Box[ChoiceletStateHolder] = {
//            s match {
//                case Stream.Empty ⇒ found
//                case head #:: tail ⇒
//                    head match {
//                        case sth @ ChoiceletStateHolder(_, hindex) if !hindex.closed && hindex.sid == sid ⇒
//                            val nextfound = found match {
//                                case foundbox @ Full(prevfound) ⇒
//                                    if (prevfound.stindex.stime < hindex.stime) {
//                                        // The previously found one is older than the one in head
//                                        prevfound close ()
//                                        Full(sth)
//                                    }
//                                    else {
//                                        // The previously found one is newer, so keep it and close head
//                                        sth close ()
//                                        foundbox
//                                    }
//                                case _ : EmptyBox ⇒ Full(sth)
//                            }
//                            mostRecentOpenState (sid, tail, nextfound)
//                        case _ ⇒
//                            head close ()
//                            mostRecentOpenState (sid, tail, found)
//                    }
//            }
//        }
    }

    sealed case class AjaxStateLog(name : String, version : String, tstamp : Long, args : JValue)
        extends AjaxApiRequest("event", "statelog") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            ChoiceletLogger.create(sclient, name, version, tstamp, args) flatMap { logger ⇒
                sclient putActivity ("guievent", logger)
                SuccessResponse
            }
        }
    }

    sealed case class AjaxCloseState(path : Option[String], id : Option[Long]) extends AjaxApiRequest("event", "closeState") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val result = path match {
                case Some(pathstr) ⇒
                    // There used to be a unique path to a Choicelet state, but this is no longer true.
                    // The state folder still has a unique id though, so get the resource id of the
                    // given path and see if it works.
                    Cfs.withValidPath(pathstr) {
                        case abspath : CfsAbsolutePath ⇒
                            Cfs.withExistingFile(abspath, SystemPrincipal, CfsOpenOptions.Default) {
                                case folder : CfsFolder ⇒
                                    // Could be a state folder, but ChoiceletLib will verify.
                                    ChoiceletLib(self).closeChoiceletState(folder.getResourceId.id)
                            }
                        case _ ⇒ Failure(s"$pathstr is not an absolute path")
                    }
                case None ⇒
                    id match {
                        case Some(stateId) ⇒
                            ChoiceletLib(self).closeChoiceletState(stateId)
                        case None ⇒
                            // This is the usual case, when a Choicelet invokes this operation.
                            sclient.withActivity ("guievent") {
                                case logger : ChoiceletLogger ⇒
                                    val chstateBox = logger.closeState()
                                    sclient closeActivity "guievent"
                                    chstateBox
                            }
                    }
            }
            // We could return the updated Choicelet state, but maintain backward compatibility
            // and return a boolean.
            result flatMap (_ ⇒ MapResponse(Map("status" → 1, "result" → true)))
        }
    }

    /**
      * Object within JSON-encoding of arguments for the uploadState operation. Contains
      * information about the login session associated with the log being uploaded.
      *
      * @param sessionid the login session id, which is the ResourceId of its session folder
      * @param cgid the ResourceId of the current login group
      * @param groups information about the groups associated with current user
      * @param user information about the current user
      * @param logingroup the path to the login group
      */
    case class SessionInfo(sessionid : BigInt, cgid : BigInt, groups : Array[JObject],
                           user : JObject, logingroup : String)

    /**
      * Object within JSON-encoding of arguments for the uploadState operation. Contains
      * information about the uploaded log, as well as a nested object containing information
      * about the associated login session.
      *
      * @param name the Choicelet name
      * @param version the Choicelet version
      * @param args initial arguments passed to the Choicelet
      * @param sinfo login session information
      * @param tstamp client timestamp at start of log
      * @param closed true if the log was closed, meaning the Choicelet was completed
      * @param uploaded true if the log has been successfully uploaded (should be false)
      * @param sid the client's unique id for this log
      */
    case class LogInfo(name : String, version : String, args : JValue, sinfo : SessionInfo,
                       tstamp : BigInt, closed : Boolean, uploaded : Boolean, sid : Option[Int])

    sealed case class AjaxUploadState(loginfo : LogInfo, events : Array[JObject]) extends AjaxApiRequest("event", "uploadState") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val fromSessionId = loginfo.sinfo.sessionid.toLong
            if (fromSessionId == sclient.sessionId.toLong) {
                // Log is coming from the client which generated it.
                ChoiceletLogger.create(sclient, loginfo.name, loginfo.version,
                                       loginfo.tstamp.toLong, loginfo.args) flatMap { logger ⇒
                    val stateId = logger.getStateId
                    sclient putActivity ("guievent", logger)
                    val sbuilder = new StringBuilder(100 * events.length)
                    (events map compactRender).addString(sbuilder, "", ",\n", ",\n")
                    val result = logger put sbuilder.toString
                    sclient closeActivity "guievent"
                    if (loginfo.closed) systemChoiceletLib.closeChoiceletState(stateId)
                    result flatMap (b ⇒ MapResponse(Map("status" → 1, "size" → sbuilder.length, "put" → b)))
                }
            }
            else {
                // TODO:
                // Need to verify that the current user has some access right
                // for the group to which the logged user belongs. Maybe just
                // the ability to add users to the group. Maybe a new kind of
                // right just for this operation.
                Cfs.withExistingFile(loginfo.sinfo.logingroup, self, CfsOpenOptions.Default) {
                    case ginfo : GroupInfo ⇒
                        GroupInfo.canAddUser(ginfo) { () ⇒
                            // Get the session id for the login session, and try to open its SessionFolder
                            val sid = loginfo.sinfo.sessionid.toLong
                            SessionFolder.getSessionById(ResourceId(sid), SystemPrincipal) flatMap { sfolder ⇒
                                try {
                                    ChoiceletLogger.create(sfolder, loginfo.name, loginfo.version,
                                        loginfo.tstamp.toLong, loginfo.args) flatMap { logger ⇒
                                        try {
                                            val sbuilder = new StringBuilder(100 * events.length)
                                            (events map compactRender).addString(sbuilder, "", ",\n", ",\n")
                                            logger put sbuilder.toString()
                                            MapResponse(Map("json_size" → sbuilder.length, "status" → 1))
                                        }
                                        finally {
                                            logger close ()
                                        }
                                    }
                                }
                                finally {
                                    // Close the session folder, since we opened it
                                    sfolder close ()
                                }
                            }
                        }
                }
            }
        }
    }

    sealed case class AjaxGetDeluxe(client : Long, id : Long) extends AjaxApiRequest("event", "dlxget") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val choiceletLib = ChoiceletLib(self)
            val boxedMap = choiceletLib.getDeluxe(client, id)
            boxedMap flatMap MapResponse
        }
    }

    sealed case class AjaxSaveDeluxe(client : Long, id : Long, data : String) extends AjaxApiRequest("event", "dlxsave") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val choiceletLib = ChoiceletLib(self)
            choiceletLib.saveDeluxe(client, id, data) flatMap MapResponse
        }
    }

    val optable : Map[String, AjaxApiRequestExtractor[AjaxRequest]] = Map (
        "findClients" → AjaxApiRequestExtractor[AjaxFindClients]("event", "findClients"),
        "clientSummary" → AjaxApiRequestExtractor[AjaxClientSummary]("event", "clientSummary"),
        "stateSummary" → AjaxApiRequestExtractor[AjaxStateSummary]("event", "stateSummary"),
        "findState" → AjaxApiRequestExtractor[AjaxFindState]("event", "findState"),
        "loadState" → AjaxApiRequestExtractor[AjaxLoadState]("event", "loadState"),
        "getState" → AjaxApiRequestExtractor[AjaxGetState]("event", "getState"),
        "statelog" → AjaxApiRequestExtractor[AjaxStateLog]("event", "statelog"),
        "closeState" → AjaxApiRequestExtractor[AjaxCloseState]("event", "closeState"),
        "uploadState" → AjaxApiRequestExtractor[AjaxUploadState]("event", "uploadState"),
        "dlxget" → AjaxApiRequestExtractor[AjaxGetDeluxe]("event", "dlxget"),
        "dlxsave" → AjaxApiRequestExtractor[AjaxSaveDeluxe]("event", "dlxsave")
    )

    val requestExtractor = new AjaxRequestTable(optable)

    def handleOp(req : Req) : Box[LiftResponse] = {
        val op = S.param("op")
        op match {
            case Full("eventlog") ⇒
                SessionClient.withSessionState(requireLogin = false) { ss ⇒
                    ss.withActivity ("guievent") {
                        case logger : ChoiceletLogger ⇒
                            req.json match {
                                case Full(JArray(list)) ⇒
                                    list map logger.logEvent collect {
                                        case f : Failure ⇒ f
                                        case Empty ⇒ Failure("logEvent returned Empty")
                                    } match {
                                        case Nil ⇒ NilResponse
                                        case head :: Nil ⇒ FailureResponse(head)
                                        case head :: _ ⇒
                                            FailureResponse(head)
                                    }
                                case Full(jv) ⇒
                                    logger logEvent jv match {
                                        case Full(_) ⇒ NilResponse
                                        case f : Failure ⇒ FailureResponse(f)
                                        case Empty ⇒ SimpleResponse(-1, "logEvent returned Empty")
                                    }
                                case e : EmptyBox ⇒
                                    val err = (e ?~ "Empty").messageChain
                                    val user = ss.getPrincipal.getPrincipalPath.toString
                                    val ip = ss.clientIpAddress
                                    Log.error(s"invalid JSON in eventlog from $user at $ip: $err")
                                    e
                            }
                    }
                }
            case Empty ⇒ requestExtractor.getReq(req) match {
                case Full(ajax) ⇒ ajax processRequest req
                case f : Failure ⇒ FailureResponse(f)
                case Empty ⇒ SimpleResponse(-1, "missing event operation")
            }
            case _ ⇒ InvalidOperation
        }
    }
}
