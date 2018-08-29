/**
  * Copyright © 2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Created by Hep on 7/6/2017.
  */
package choice.script

import java.nio.file.StandardOpenOption
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.atomic.AtomicBoolean

import choice.access.{Principal, SystemPrincipal}
import choice.actor.SessionManager
import choice.fs._
import choice.lib.{EventOps, FileOps}
import choice.script.ChoiceletLib.Log
import net.liftweb.actor.LiftActor
import net.liftweb.common._
import net.liftweb.json.JsonAST.{JArray, JInt}
import net.liftweb.json._
import net.liftweb.util.Helpers.{millis, tryo}
import net.liftweb.util.{Helpers, Schedule, TimeHelpers}

import scala.collection.mutable
import scala.util.Sorting

class ChoiceletLib(implicit providedPrincipal : () ⇒ Principal) { chlib ⇒

    import ChoiceletLib.simpleDate
    implicit val formats : DefaultFormats.type = DefaultFormats

    protected var _systemNestingLevel : Int = 0
    protected implicit def principal : Principal = if (_systemNestingLevel > 0) SystemPrincipal else providedPrincipal()

    /**
      * Some things need to be done as SystemPrincipal, particular activities related to
      * maintaining the session cache.
      *
      * @param f a function to run as SystemPrincipal
      * @tparam T its return type
      * @return whatever the function returns
      */
    protected def asSystem[T](f : () ⇒ T) : T = {
        _systemNestingLevel += 1
        val result = f()
        _systemNestingLevel -= 1
        result
    }

    /**
      * Find the state folder for a given state id, and read its Index file, returning the
      * path to the state folder and the deserialized Choicelet state.
      *
      * @param stateId the state id
      * @return a boxed tuple of (state folder path, deserialized state)
      */
    def getExistingStateInfo(stateId : Long) : Box[(CfsAbsolutePath, ChoiceletState)] = {
        Cfs.open(CfsVFileId(stateId), principal, CfsOpenOptions.Default) match {
            case Full(stfolder : CfsFolder) ⇒
                // The stateId references a folder. That's a good start, but it could be any folder.
                // See if any of its paths are under the folder where Choicelet sessions are stored.
                val allPaths = stfolder.findPaths.toArray
                val pindex = allPaths.map(_.toString).indexWhere(_.startsWith(ChoiceletLib.ChoiceletFolder))
                val result = if (pindex >= 0) {
                    val likelyPath = allPaths(pindex)
                    // It's looking more like a state folder, but if it is, it will have an Index member.
                    // Read the Index file and check the state id in it, just to be sure.
                    readChoiceletStateIndex(likelyPath) match {
                        case Full(chstate) ⇒
                            if (chstate.id == stateId) Full((likelyPath, chstate))
                            else Failure(s"$stateId is not a valid Choicelet state")
                        case e : EmptyBox ⇒
                            Failure(s"$stateId is not a valid Choicelet state", Empty, Full(e ?~ "Empty"))
                    }
                }
                else Failure(s"$stateId is not a valid Choicelet state")
                stfolder close ()
                result
            case Full(other) ⇒
                other close ()
                Failure(s"file id $stateId is not a Choicelet state folder")
            case Empty ⇒
                Failure(s"file id $stateId does not exist")
            case f : Failure ⇒ f
        }
    }

    /**
      * Read the Index file of a Choicelet state, returning it deserialized to a ChoiceletState.
      * The state path is assumed to be from a reliable source.
      *
      * @param stpath the path to the Choicelet state folder
      * @return a boxed Choicelet state
      */
    def readChoiceletStateIndex(stpath : CfsAbsolutePath) : Box[ChoiceletState] = tryo {
        val indexPath = stpath / "Index"
        val reader = CfsFiles.newBufferedReader(indexPath)(() ⇒ principal)
        try {
            Serialization.read[ChoiceletState](reader)
        }
        finally {
            reader close()
        }
    }

    /**
      * Write the "Index" file for a Choicelet state, given the path to the Choicelet state folder
      * and the Choicelet state to be written.
      *
      * @param stpath path to the Choicelet state folder
      * @param chstate Choicelet state to be serialized to the Index member
      * @return the boxed Choicelet state
      */
    def writeStateIndex(stpath : CfsAbsolutePath, chstate : ChoiceletState) : Box[ChoiceletState] = {
        val indexPath = stpath / "Index"
        tryo {
            val writer = CfsFiles.newBufferedWriter(indexPath, MIME_TYPE("application/json"),
                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
                StandardOpenOption.WRITE)(() ⇒ principal)
            try {
                Serialization.write(chstate, writer)
                chstate
            }
            finally {
                writer close ()
            }
        }
    }

    /**
      * Read the Events file for a Choicelet state, using a path to the state folder which is
      * assumed to come from a reliable source. The contents of the file are parsed into a
      * list of JValues, each of which represents an event.
      *
      * @param stpath path to Choicelet state folder
      * @return boxed list of event JValues
      */
    def readEvents(stpath : CfsAbsolutePath) :  Box[List[JValue]] = tryo {
        val reader = CfsFiles.newBufferedReader(stpath / "Events")(() ⇒ principal)
        val eventsStr = try {
            val s = reader.readAll()
            if (s.length > 0 && s(s.length - 1) == ',') s.substring(0, s.length - 1) else s
        }
        finally {
            reader close ()
        }
        JsonParser parse s"[$eventsStr]" match {
            case JArray(list) ⇒ list
            case _ ⇒ Nil
        }
    }

    /**
      * Create a writer for a Choicelet state's Events file. The path to the Choicelet state's
      * folder is assumed to have been obtained from a reliable source or otherwise validated.
      *
      * @param stpath path to the Choicelet state folder
      * @return a boxed writer
      */
    def newEventsWriter(stpath : CfsAbsolutePath) : Box[CfsBufferedWriter] = tryo {
        // The file is actually only JSON-ish. Specifically it is a JSON encoding of an array
        // of objects, but without the enclosing [ ] of the array. Each object is a logged
        // event.
        CfsFiles.newBufferedWriter(stpath / "Events", MIME_TYPE("application/json"),
            StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
            StandardOpenOption.WRITE)(() ⇒ principal)
    }

    /**
      * Get a Choicelet home handle for a Choicelet identified by its model name and
      * version string.
      *
      * @param name the Choicelet model name
      * @param version the Choicelet version string
      * @return a handle for the Choicelet home folder
      */
    def getChoiceletHome(name : String, version : String) : Box[ChoiceletHomeHandle] = {
        ChoiceletHomeImpl(name, version)
    }

    /**
      * Get a Choicelet home handle for a Choicelet identified by its unique id. This
      * id is sometimes called the "client id" or "choicelet id". (Currently it is
      * the resource id of the Choicelet home folder.)
      *
      * @param id the unique Choicelet id, for a particular Choicelet version
      * @return a handle for the Choicelet home folder
      */
    def getChoiceletHome(id : Long) : Box[ChoiceletHomeHandle] = {
        ChoiceletHomeImpl(id)
    }

    /**
      * Get information about Choicelets which match a given query. Note that the
      * objects in the returned list must be closed by the caller.
      *
      * @param query specifies characteristics of the Choicelets of interest
      * @return a list of Choicelet home handles for matching Choicelets
      */
    def getChoicelets(query : ChoiceletSpec) : List[ChoiceletHomeHandle] = {
        query.client orElse query.id match {
            case Some(id) ⇒
                ChoiceletHomeImpl(id) match {
                    case Full(chhome) ⇒
                        if (query.name.fold (true) (_ == chhome.name) &&
                            query.version.fold (true) (_ == chhome.version)) {
                            List(chhome)
                        }
                        else {
                            chhome close ()
                            Nil
                        }
                    case e : EmptyBox ⇒
                        Log.error((e ?~ "Empty").msg)
                        Nil
                }
            case None ⇒
                // No id, so get the folder that matches the name if specified, or else
                // get folders for all Choicelet names.
                val folderListBox = Cfs.withExistingFile(ChoiceletLib.ChoiceletFolder,
                                                         principal, CfsOpenOptions.Default) {
                    case choicelet : CfsFolder ⇒
                        query.name match {
                            case Some(chname) ⇒
                                choicelet getMember chname match {
                                    case Full(chfolder : CfsFolder) ⇒ Full(List(chfolder))
                                    case Full(other) ⇒
                                        other close ()
                                        Failure(s"$chname is not a folder")
                                    case Empty ⇒ Full(Nil)
                                    case f : Failure ⇒ f
                                }
                            case _ ⇒ Full(choicelet.getFolders)
                        }
                }
                val matchedFolders = folderListBox flatMap { namedFolders ⇒
                    // For each of the named folders, get the folder that matches the version,
                    // if specified, or else get the folders for all versions.
                    val versionFolders = namedFolders flatMap { fname ⇒
                        val result = query.version match {
                            case Some(chversion) ⇒
                                fname getMember chversion match {
                                    case Full(chhome : CfsFolder) ⇒ List(chhome)
                                    case Full(other) ⇒
                                        other close ()
                                        Nil
                                    case _ : EmptyBox ⇒ Nil
                                }
                            case _ ⇒ fname.getFolders
                        }
                        fname close ()
                        result
                    }
                    Full(versionFolders)
                }
                // Finally read the Choicelet index for each Choicelet home that matched
                matchedFolders match {
                    case Full(folderList) ⇒
                        folderList flatMap { folder ⇒
                            ChoiceletHomeImpl(folder) match {
                                case Full(chhome) ⇒ List(chhome)
                                case e : EmptyBox ⇒
                                    folder close ()
                                    Log.error((e ?~ "Empty").msg)
                                    Nil
                            }
                        }
                    case _ : EmptyBox ⇒ Nil
                }
        }
    }

//    /**
//      * Extract a query from a JSON object. This is assumed to be a simple object
//      * with fields containing strings, numbers, or booleans.
//      *
//      * @param query an internal representation of a JSON object
//      * @return a map of field names to values
//      */
//    def extractQuery(query : Option[JObject]) : Map[String, Any] = {
//        (query fold Map[String, Any]()) { jo ⇒
//            jo.values collect {
//                case (key, s : String) ⇒ (key, s)
//                case (key, n : Int) ⇒ (key, n.toLong)
//                case (key, n : Long) ⇒ (key, n)
//                case (key, n : BigInt) ⇒ (key, n.toLong)
//                case (key, b : Boolean) ⇒ (key, b)
//            }
//        }
//    }

    /**
      * For each Choicelet matching a given query, return a map containing Choicelet
      * identification information and a count of the number of sessions (state) of
      * the Choicelet which match other terms of the query.
      *
      * @param query a query containing search terms for both Choicelets and
      *              Choicelet sessions
      * @return list of maps
      */
    def getChoiceletSummary(query : ChoiceletSummarySpec) : List[Map[String, Any]] = {
        getChoicelets(query.getChoiceletSpec) map { chhome ⇒
            try {
                val chstateStream = chhome.getChoiceletStateStream(query.id)
                val stcount = (chstateStream foldLeft 0) { (count, chstate) ⇒
                    val nextcount =
                        if (query.matchState(chstate, principal)) count + 1
                        else count
                    nextcount
                }
                chhome.asMap + ("count" → stcount)
            }
            finally {
                chhome close ()
            }
        }
    }

    def getStateGroupDescription(chstate : ChoiceletState) : Box[(Long, String, Option[String])] = {
        // The userPath is considered most definitive, otherwise the original user
        val upath = chstate.userPath getOrElse chstate.user
        Cfs.withExistingFile(upath, principal, CfsOpenOptions.Default) {
            case uinfo : UserInfo ⇒
                val ginfo = uinfo.getGroupInfo
                Full((ginfo.getResourceId.id, ginfo.getName, ginfo.getDescription))
        }
    }

    def getStateSummary(query : ChoiceletSummarySpec) : List[Map[String, Any]] = {
        getChoicelets(query.getChoiceletSpec) map { chhome ⇒
            val gmapmap = collection.mutable.Map[Long, Map[String, Any]]()
            val groupMap = collection.mutable.Map[Long, Int]()
            val dateMap = collection.mutable.Map[String, Int]()
            try {
                chhome.getChoiceletStateStream(query.id) foreach { chstate ⇒
                    if (query.matchState(chstate, principal)) {
                        val groupId = getStateGroupDescription(chstate) map {
                            case (gid, gname, gdesc) ⇒
                                gmapmap getOrElseUpdate(gid, Map(
                                    "id" → gid,
                                    "name" → gname,
                                    "desc" → gdesc
                                ))
                                gid
                        }
                        val date = simpleDate.format(new Date(chstate.stime))
                        groupId foreach { key ⇒
                            groupMap put(key, 1 + (groupMap getOrElseUpdate(key, 0)))
                        }
                        dateMap put(date, 1 + (dateMap getOrElseUpdate(date, 0)))
                    }
                }
                // Convert that map to a form suitable for the response
                val dateMapList : Array[Map[String, Any]] = dateMap.toArray.sortBy(_._1) map { pair ⇒
                    val (date, count) = pair
                    Map[String, Any]("date" → date, "count" → count)
                }
                val groupMapList = (gmapmap map { pair ⇒
                    val (gid, gmap) = pair
                    gmap + ("count" → (groupMap get gid))
                }).toList
                chhome.asMap + ("dates" → dateMapList) + ("groups" → groupMapList)
            }
            finally {
                chhome close ()
            }
        }
    }

    def getStateMap(chstate : ChoiceletState) : Map[String, Any] = {
        val userPath = chstate.userPath getOrElse chstate.user

        val (groupPath, username) = userPath lastIndexOf '/' match {
            case i if i > 0 ⇒ (userPath substring (0, i), userPath substring (i + 1))
            case i if i == 0 ⇒ ("/", userPath substring 1)
            case _ ⇒ ("", userPath)
        }

        val duration = chstate.endClient map (_ - chstate.tstamp)

        Map("id" → chstate.id,
            "username" → username,
            "userid" → chstate.userid,
            "groupname" → groupPath,
            "jsessionid" → chstate.jsessionid,
            "client" → chstate.client,
            "session" → chstate.sid,
            "closed" → chstate.closed,
            "start" → chstate.stime,
            "altid" → chstate.altid,
            "duration" → duration.getOrElse(0L),
            "startClient" → chstate.tstamp,
            "endClient" → chstate.endClient.getOrElse(chstate.tstamp),
            "hasDeluxe" → chstate.hasDeluxe.getOrElse(false)
        )
    }

    def findState(query : ChoiceletSummarySpec) : Box[Array[Map[String, Any]]] = {
        val matchedChoicelets = getChoicelets(query.getChoiceletSpec)
        val matchedStates = matchedChoicelets flatMap { chhome ⇒
            try {
                chhome.getChoiceletStateStream(query.id) filter { chstate ⇒
                    query.matchState(chstate, principal)
                }
            }
            finally {
                chhome close()
            }
        }
        // This is very memory intensive and may throw exceptions due to lack of memory
        // or too much unproductive garbage collection. Adding more memory to the JVM
        // seems to solve the problem. In any case, even with an exception there should
        // be a response.
        tryo {
            val sortedStateArray = Sorting.stableSort(matchedStates, (chstate : ChoiceletState) ⇒ chstate.stime)
            sortedStateArray map getStateMap
        }
    }

    /**
      * Mark a given Choicelet state as "closed". This is typically done when a Choicelet
      * has been completed.
      *
      * @param stateId the unique state id of the Choicelet state
      * @return the boxed, updated state object
      */
    def closeChoiceletState(stateId : Long) : Box[ChoiceletState] = {
        getExistingStateInfo(stateId) flatMap {
            case (stpath, chstate) ⇒
                writeStateIndex(stpath, chstate.copy(closed = true))
        }
    }

    def withChoiceletFolder[T](f : CfsFolder ⇒ Box[T]) : Box[T] = {
        Cfs.withExistingFile(ChoiceletLib.ChoiceletFolder, principal, CfsOpenOptions.Default) {
            case folder : CfsFolder ⇒ f(folder)
        }
    }

    def findOrCreateChoiceletHome(name : String, version : String) : Box[ChoiceletHomeHandle] =
        ChoiceletHomeImpl.getOrCreate(name, version)

    def findChoiceletVersions(name : String) : List[ChoiceletHomeHandle] =
        getChoicelets(ChoiceletSpec(None, Some(name), None, None))

    /**
      * Check whether a session might still be active, based on whether its associated
      * HTTP session is still active.
      *
      * @param chstate a ChoiceletState object for the session
      * @return true if the associated HTTP session is still active, false otherwise
      */
    def isActiveSession(chstate : ChoiceletState) : Boolean = {
        SessionManager.getSessionInfo(chstate.jsessionid).isDefined
    }

    def getDeluxe(client : Long, id : Long) : Box[Map[String, Any]] = {
        getExistingStateInfo(id) match {
            case Full((stpath, chstate)) if chstate.client == client ⇒
                Cfs.open(stpath / "Deluxe", principal, CfsOpenOptions.Default) match {
                    case Full(deluxe : CfsPlain) ⇒
                        val result = tryo {
                            val deluxeMap = deluxe.asMap
                            val reader = CfsFiles.newBufferedReader(deluxe)
                            try {
                                deluxeMap + ("data" → reader.readAll())
                            }
                            finally {
                                reader close()
                                deluxe.setExpectedClosed()
                            }
                        }
                        deluxe close()
                        result
                    case Full(other) ⇒
                        other close()
                        Failure(s"deluxe log for Choicelet state id $id is an unexpected file type")
                    case Empty ⇒
                        Full(Map("status" → 0, "msg" → s"Deluxe does not exist for Choicelet session id $id"))
                    case f : Failure ⇒
                        Failure(s"Error accessing Deluxe for Choicelet state id $id", Empty, Full(f))
                }
            case Full(_) ⇒ Failure(s"Choicelet id $client has no state with id $id")
            case e : EmptyBox ⇒ e
        }
    }

    def saveDeluxe(client : Long, id : Long, data : String) : Box[Map[String, Any]] = {
        getExistingStateInfo(id) match {
            case Full((stpath, chstate)) if chstate.client == client ⇒
                // Write the Deluxe file
                val deluxePath = stpath / "Deluxe"
                val boxlen = tryo {
                    val writer = CfsFiles.newBufferedWriter(deluxePath, MIME_TYPE("application/json"),
                        StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
                        StandardOpenOption.WRITE)(() ⇒ principal)
                    try {
                        writer write data
                        data.length
                    }
                    finally {
                        writer close ()
                    }
                }
                // Update the hasDeluxe flag
                val updatedChstate = chstate.copy(hasDeluxe = Some(true))
                // ... in the cache
                ChoiceletCacheManager.updateChoiceletState(updatedChstate)
                // ... and in the original Index file for the Choicelet state
                boxlen flatMap { _ ⇒
                    Cfs.withExistingFile(deluxePath, principal, CfsOpenOptions.Default) {
                        case plain : CfsPlain ⇒
                            asSystem { () ⇒
                                tryo(writeStateIndex(stpath, updatedChstate))
                            }
                            Full(plain.asMap)
                    }
                }
            case Full(_) ⇒ Failure(s"Choicelet id $client has no state with id $id")
            case Empty ⇒
                Failure(s"Choicelet id $client does not have a state folder for id $id")
            case f : Failure ⇒ f
        }
    }

    def ensureSessionFolderLink(stpath : CfsAbsolutePath, chstate : ChoiceletState) : Unit = asSystem { () ⇒
        val (chname, chversion) = {
            val parts = stpath.toString.substring(ChoiceletLib.ChoiceletFolder.length + 1).split('/')
            (parts(0), parts(1))
        }
        Cfs.withExistingFile(stpath, principal) {
            case stateFolder : CfsFolder ⇒
                val vnode = stateFolder.getVnode
                // If the Choicelet state folder is already linked in more than one place,
                // assume it's already linked under the associated SessionFolder
                val nlinks = vnode.withReadLock { () ⇒ vnode.getResource.getRefCount }
                if (nlinks < 2) {
                    // Get the session id associated with this Choicelet state, and open the
                    // corresponding SessionFolder (or try to).
                    SessionFolder getSessionById (chstate.sid, principal) match {
                        case Full(sessionFolder) ⇒
                            try {
                                val stname = s"${chstate.id}-$chname-$chversion"
                                sessionFolder link(stname, stateFolder) foreach (_ close())
                                Empty
                            }
                            finally {
                                sessionFolder close()
                            }
                        case _ : EmptyBox ⇒
                            val stateDesc = s"$chname $chversion state ${chstate.id}"
                            Log.error(s"$stateDesc references non-existent session ${chstate.sid}")
                            Empty
                    }
                }
                else Empty
        }
    }

    /**
      * ChoiceletLib implementation of a ChoiceletHomeHandle.
      *
      * @param chhome the home folder of a particular Choicelet version
      * @param name the Choicelet model name
      * @param version the Choicelet version string
      */
    private class ChoiceletHomeImpl(protected val chhome : CfsFolder,
                                    override val name : String, override val version : String)
        extends ChoiceletHomeHandle {

        /** The unique id of the Choicelet **/
        override val id : Long = chhome.getResourceId.id

        /**
          * Close this handle.
          */
        override def close() : Unit = {
            super.close()
            chhome close()
        }

        /**
          * Get a stream of handles for sessions of this Choicelet. If the id is specified, the
          * returned stream will contain only the indicated session.
          *
          * @param id the unique id of a session for this Choicelet, aka "state id"
          * @return
          */
        override def getChoiceletStateStream(id : Option[Long]) : Stream[ChoiceletState] = {
            id match {
                case Some(stateId) ⇒
                    // Just a particular session wanted
                    getExistingStateInfo(stateId) match {
                        case Full((_, chstate)) ⇒ Stream(chstate)
                        case _ : EmptyBox ⇒ Stream.Empty
                    }
                case None ⇒
                    // All sessions wanted. First look for a session cache file.
                    readUpdatedSessionCache() map (_.toStream) openOr Stream.Empty
            }
        }

        /**
          * Update the session cache and return its contents as an array of ChoiceletState objects.
          *
          * Active sessions are ignored.
          *
          * If the session cache does not exist, it will be created.
          *
          * Newly cached sessions are moved to the "archive" sub-folder of the Choicelet home.
          *
          * @return boxed array of ChoiceletState objects
          */
        def readUpdatedSessionCache() : Box[Array[ChoiceletState]] = {
            import ChoiceletCacheManager.sessionCacheName

            // Update the session cache and get its contents
            ChoiceletCacheManager.getSessionCache(chhome) match {
                case full @ Full(_) ⇒
                    // Verify that the current principal has read access to the session cache file
                    chhome open (sessionCacheName, principal, CfsOpenOptions.Default) match {
                        case Full(scache : CfsPlain) ⇒
                            try {
                                (CfsPlain canGetInputStream scache) { () ⇒ full }
                            }
                            finally {
                                scache close ()
                            }
                        case Full(other) ⇒
                            other close ()
                            Failure(s"${chhome.getPath.toString}/$sessionCacheName is not a plain file")
                        case Empty ⇒
                            Failure(s"${chhome.getPath.toString}/$sessionCacheName does not seem to exist")
                        case f : Failure ⇒ f
                    }
                case e : EmptyBox ⇒ e
            }
        }

        /**
          * Create a new state for this Choicelet. A Choicelet state is represented by a folder
          * containing various information related to a particular Choicelet session:
          *
          *     Index - file containing JSON-serialization of ChoiceletState
          *     Events - JSON-ish file containing events suitable for Choicelet playback
          *     Deluxe - a text file generated sometime after the session, containing a detailed
          *              account of what transpired during the Choicelet session
          *
          * A state folder is initially created in the Choicelet home folder. Sometime after the
          * session, the Choicelet state is updated and added to a session cache file. The state
          * folder is then moved to a "archive" sub-folder of the Choicelet home. State folders
          * associated with active HTTP sessions are not cached until they are either marked as
          * "closed" (meaning the Choicelet was completed), or the HTTP session ends.
          *
          * @param sfolder the session folder for the HTTP session
          * @param tstamp  the client's timestamp of the start of the session
          * @param args    Choicelet model arguments, JSON-encoded
          * @param closed  a flag indicating whether this session has been closed
          * @param stime   the server's timestamp of the start of the session
          * @param altid   the id of this session under Choice, if it was migrated from Choice
          * @return boxed Choicelet state
          */
        def createChoiceletState(sfolder : SessionFolder, tstamp : Long, args : String,
                                 closed : Boolean = false, stime : Long = millis,
                                 altid : Option[Long] = None) : Box[(CfsAbsolutePath, ChoiceletState)] = {
            sfolder.getData flatMap { sinfo ⇒
                val sid = sfolder.getResourceId.id
                val jsessionid = sinfo.sessionId
                val userid = sinfo.userId
                val upath = sinfo.userPath
                val chsfolder = Cfs.createUniqueSubfolder(chhome)
                val stateId = chsfolder.getResourceId.id
                val stpath = chsfolder.getPath

                val chstate = ChoiceletState(stateId, sid, upath, userid, jsessionid,
                                             id, closed = closed, stime, tstamp, args, altid)
                val result = writeStateIndex(stpath, chstate) map ((stpath, _))

                // Link the new Choicelet state into the user's session folder as well
                val sfname = s"$stateId-$name-$version"
                sfolder linkUnchecked (sfname, chsfolder) foreach (_ close())
                chsfolder close ()
                // Clear the in-memory session cache, so that the session cache file will be updated
                // on the next query.
                ChoiceletCacheManager.clearCacheEntry(id)
                result
            }
        }
    }

    object ChoiceletHomeImpl {
        implicit val formats : DefaultFormats.type = DefaultFormats

        def apply(chhome : CfsFolder) : Box[ChoiceletHomeHandle] = {
            val homePath = chhome.getPath.toString
            val nvbox = {
                val parts = homePath.substring(ChoiceletLib.ChoiceletFolder.length + 1).split('/')
                if (parts.length == 2) {
                    Full((parts(0), parts(1)))
                }
                else Failure(s"${chhome.getPath.toString} is not a Choicelet home folder")
            }
            nvbox map {
                case (name, version) ⇒ new ChoiceletHomeImpl(chhome, name, version)
            }
        }

        /**
          * Create a Choicelet home handle, given the Choicelet model name and version string.
          *
          * @param name    Choicelet model name
          * @param version Choicelet version string
          * @return a boxed Choicelet home handle
          */
        def apply(name : String, version : String) : Box[ChoiceletHomeHandle] = {
            Cfs.withValidPath(s"${ChoiceletLib.ChoiceletFolder}/$name/$version") { chpath ⇒
                Cfs.withExistingFile(chpath, principal) {
                    case chhome : CfsFolder ⇒
                        Full(new ChoiceletHomeImpl(chhome, name, version))
                }
            }
        }

        /**
          * Create a Choicelet home handle, given the unique id of the Choicelet. The unique id
          * is actually the Resource id of the Choicelet home folder.
          *
          * @param id Choicelet unique id
          * @return a boxed Choicelet home handle
          */
        def apply(id : Long) : Box[ChoiceletHomeHandle] = {
            val result = Cfs.open(CfsVFileId(id), principal, CfsOpenOptions.Default) match {
                case Full(chhome : CfsFolder) ⇒
                    chhome.findPaths.map(_.toString).find(_.startsWith(ChoiceletLib.ChoiceletFolder)) match {
                        case Some(hpath) ⇒
                            val parts = hpath.substring(ChoiceletLib.ChoiceletFolder.length + 1) split '/'
                            if (parts.length == 2) {
                                Full(new ChoiceletHomeImpl(chhome, parts(0), parts(1)))
                            }
                            else {
                                chhome close ()
                                Empty
                            }
                        case None ⇒
                            chhome close ()
                            Empty
                    }
                case Full(other) ⇒
                    other close ()
                    Empty
                case e : EmptyBox ⇒ e
            }
            result ?~ s"file id $id is not a Choicelet home folder"
        }

        /**
          * Create a Choicelet home handle, given the Choicelet model name and version string. If the
          * Choicelet does not already have a home folder, one is created for it.
          *
          * Previous implementations created an "Index" JSON file in the Choicelet home folder when
          * the folder was created. But the Index file did not contain any information which could
          * not be derived from the home folder itself. There may yet be a use for an Index file, but
          * it would be created and accessed via the ChoiceletHomeHandle, not here.
          *
          * @param name    Choicelet model name
          * @param version Choicelet version string
          * @return a boxed Choicelet home handle
          */
        def getOrCreate(name : String, version : String) : Box[ChoiceletHomeHandle] = {
            Cfs.withValidPath(s"${ChoiceletLib.ChoiceletFolder}/$name/$version") { chpath ⇒
                FileOps.getOrMakeFolder(chpath, principal, recursive = true) match {
                    case Full(chhome) ⇒
                        Full(new ChoiceletHomeImpl(chhome, name, version))
                    case Empty ⇒ Failure("getOrMakeFolder returned Empty")
                    case f : Failure ⇒ f
                }
            }
        }
    }
}

object ChoiceletLib {
    implicit val formats : DefaultFormats.type = DefaultFormats

    val Log = Logger("choice.script.ChoiceletLib")
    val simpleDate = new SimpleDateFormat("yyyy-MM-dd")
    val ChoiceletFolder = "/System/Choicelet"

    def apply(principal : Principal) : ChoiceletLib = {
        new ChoiceletLib()(() ⇒ principal)
    }

    def apply(principal : () ⇒ Principal) : ChoiceletLib = {
        new ChoiceletLib()(principal)
    }
}

/**
  * An in-memory cache of a Choicelet's session-cache.json file.
  *
  * @param chpath path to the Choicelet's home folder
  */
abstract class SessionCacheCache(chpath : CfsAbsolutePath) {
    implicit val formats : DefaultFormats.type = DefaultFormats

    private var _dirty : Boolean = false
    private var _dirtyTime : Long = 0L
    private var _lastAccessTime : Long = millis

    protected val array : Array[ChoiceletState]

    /**
      * Return the deserialized contents of session-cache.json.
      *
      * @return array of ChoiceletState
      */
    def get() : Array[ChoiceletState] = {
        _lastAccessTime = millis
        array.clone()
    }

    /**
      * Return the Choicelet state for a particular session if it's in session-cache.json.
      *
      * @param stateId the state id of the session
      * @return the ChoiceletState if found
      */
    def get(stateId : Long) : Option[ChoiceletState] = {
        _lastAccessTime = millis
        array find (_.id == stateId)
    }

    /**
      * Return the last read time of this cache entry.
      *
      * @return millisecond timestamp
      */
    def lastAccessTime : Long = _lastAccessTime

    /**
      * Return the time since the last update of this cache entry. If the entry has not been updated
      * since it was created or last written back to the file, the return value is -1.
      *
      * @return milliseconds since last update, or -1 if no updates
      */
    def lastUpdateAge : Long = if (_dirty) millis - _dirtyTime else -1L

    /**
      * Write the contents of this entry back to the session-cache.json file.
      */
    def write() : Unit = tryo {
        import ChoiceletCacheManager.sessionCacheName

        val writer = CfsFiles.newBufferedWriter(chpath / sessionCacheName, MIME_TYPE("application/json"),
                                                StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING,
                                                StandardOpenOption.WRITE)(() ⇒ SystemPrincipal)
        try {
            Serialization.write(array, writer)
        }
        finally {
            writer close ()
            _dirty = false
        }
    }

    /**
      * Update a particular ChoiceletState in this entry, if it is present.
      *
      * @param chstate the new ChoiceletState
      * @return true if the update occurred
      */
    def update(chstate : ChoiceletState) : Boolean = {
        val i = array indexWhere (_.id == chstate.id)
        val result = i >= 0
        if (result) {
            array(i) = chstate
            _dirty = true
            _dirtyTime = millis
            _lastAccessTime = _dirtyTime
        }
        result
    }
}

class ChoiceletCacheManager(chpath : CfsAbsolutePath) extends SessionCacheCache(chpath) with LiftActor {

    import EventOps.systemChoiceletLib
    import ChoiceletCacheManager._

    override protected lazy val array : Array[ChoiceletState] = {
        Cfs.withExistingFile(chpath, SystemPrincipal, CfsOpenOptions.Default) {
            case chhome : CfsFolder ⇒
                tryo(readUpdatedSessionCache(chhome))
        } openOr Array()
    }

    /**
      * Update the session cache and return its contents as an array of ChoiceletState objects.
      *
      * Active sessions are ignored.
      *
      * If the session cache does not exist, it will be created.
      *
      * Newly cached sessions are moved to the "archive" sub-folder of the Choicelet home.
      *
      * This activity is done using the SystemPrincipal so that access control is not an issue,
      * since the maintenance of the cache is a system responsibility.
      *
      * @param chhome Choicelet home folder handle
      * @return array of ChoiceletState objects
      */
    def readUpdatedSessionCache(chhome : CfsFolder) : Array[ChoiceletState] = {
        import ChoiceletCacheManager.sessionCacheName

        // Read the existing cache if any
        val cacheBox = tryo {
            val reader = CfsFiles.newBufferedReader(chhome.getPath / sessionCacheName)(() ⇒ SystemPrincipal)
            try {
                Serialization.read[Array[ChoiceletState]](reader)
            }
            finally {
                reader close()
            }
        }
        // If there was no session cache create it
        val currentCache = cacheBox openOr createSessionCache(chhome).toArray
        // Look for sessions never previously cached.
        readUncachedSessions(chhome) match {
            case Nil ⇒
                // No new sessions, hopefully the common case
                currentCache
            case updatedSessions ⇒
                // Move the new sessions to the cached folder
                chhome open ("archive", SystemPrincipal, CfsOpenOptions.Default) match {
                    case Full(cacheFolder : CfsFolder) ⇒
                        try {
                            updatedSessions foreach { chstate ⇒
                                val name = chstate.id.toString
                                chhome.withMember(name) {
                                    case sessionFolder : CfsFolder if sessionFolder.getResourceId.id == chstate.id ⇒
                                        // Link the session folder under the cached folder
                                        cacheFolder linkUnchecked(name, sessionFolder) foreach (_ close())
                                        // Unlink it from the Choicelet home folder
                                        chhome unlinkUnchecked(sessionFolder, recursive = false)
                                }
                            }
                        }
                        finally {
                            cacheFolder close ()
                        }
                    case Full(other) ⇒
                        other close ()
                        Log.error(s"${chhome.getPath.toString}/archive is not a folder")
                    case e : EmptyBox ⇒
                        Log.error(s"""${chhome.getPath.toString}/archive: ${(e ?~ "Empty").msg}""")
                }

                val combinedSessions = new Array[ChoiceletState](currentCache.length + updatedSessions.length)
                currentCache.copyToArray(combinedSessions)
                updatedSessions.copyToArray(combinedSessions, currentCache.length)
                // Update the session cache file with the new sessions
                val writer = CfsFiles.newBufferedWriter(chhome.getPath / sessionCacheName,
                                                        MIME_TYPE("application/json"), StandardOpenOption.CREATE, StandardOpenOption.WRITE,
                                                        StandardOpenOption.TRUNCATE_EXISTING)(() ⇒ SystemPrincipal)
                try {
                    Serialization.write(combinedSessions, writer)
                }
                finally {
                    writer close()
                }
                combinedSessions
        }
    }

    /**
      * Read the Index files of all Choicelet sessions in a given folder, returning a list
      * of ChoiceletState objects. Sessions which are currently active are ignored.
      *
      * @param folder the folder, typically the Choicelet home folder or its "archive" subfolder
      * @return a list of ChoiceletState for whatever sessions are found
      */
    def readUncachedSessions(folder : CfsFolder) : List[ChoiceletState] = {
        def helper(folders : List[CfsFolder], result : List[ChoiceletState]) : List[ChoiceletState] = {
            folders match {
                case Nil ⇒ result
                case stateFolder :: tail ⇒
                    val nextResult = try {
                        if (stateFolder.getName matches """\d+""") {
                            val stpath = stateFolder.getPath
                            systemChoiceletLib.readChoiceletStateIndex(stpath) match {
                                case Full(chstate) ⇒
                                    // Ignore active sessions. A closed session is assumed inactive.
                                    if (chstate.closed || !systemChoiceletLib.isActiveSession(chstate)) {
                                        fillChoiceletState(stpath, chstate) :: result
                                    }
                                    else result
                                case _ ⇒ result
                            }
                        }
                        else result
                    }
                    finally {
                        stateFolder close()
                    }
                    helper(tail, nextResult)
            }
        }

        helper(folder.getFolders, Nil)
    }

    /**
      * Fill in some fields in a Choicelet state, prior to caching it. Uncached state folders are
      * assumed to be in the Choicelet home folder. The state Index file is also updated.
      *
      * @param stpath  state folder path for the given state
      * @param chstate the current contents of the Choicelet state Index file (deserialized)
      * @return the updated state object
      */
    def fillChoiceletState(stpath : CfsAbsolutePath, chstate : ChoiceletState) : ChoiceletState = {
        // Get the endClient value as the client timestamp on the last event
        val endClient = systemChoiceletLib.readEvents(stpath).toOption flatMap { evlist ⇒
            if (evlist.isEmpty) None
            else {
                val lastEvent = evlist.last
                lastEvent \ "tstamp" match {
                    case JInt(num) ⇒ Some(num.toLong)
                    case _ ⇒ lastEvent \ "_timestamp" match {
                        case JInt(num) ⇒ Some(num.toLong)
                        case _ ⇒ None
                    }
                }
            }
        }
        val userPath =
            if (chstate.user == "/System/Users/guest") {
                val result = SessionFolder getSessionById(chstate.sid, SystemPrincipal) flatMap { sfolder ⇒
                    try {
                        sfolder.getData map (_.userPath)
                    }
                    finally {
                        sfolder close()
                    }
                }
                result openOr chstate.user
            }
            else chstate.user
        val hasDeluxe = {
            Cfs.withExistingFile(stpath / "Deluxe", SystemPrincipal) {
                case _ : CfsPlain ⇒ Full(true)
            } openOr false
        }
        val newState = chstate.copy(endClient = endClient orElse Some(chstate.tstamp),
                                    userPath = Some(userPath), hasDeluxe = Some(hasDeluxe))
        systemChoiceletLib.writeStateIndex(stpath, newState)
        systemChoiceletLib.ensureSessionFolderLink(stpath, newState)
        newState
    }

    /**
      * Create or restore the session cache. This includes only sessions which had previously
      * been moved to the "archive" folder. The caller is responsible for updating the session
      * cache with any new sessions which were not previously cached.
      *
      * It also creates the "archive" folder if it doesn't already exist.
      *
      * @return a list of ChoiceletState objects for restored sessions
      */
    def createSessionCache(chhome : CfsFolder) : List[ChoiceletState] = {
        // Create the "archive" folder if it doesn't exist
        FileOps.getOrMakeFolder(chhome.getPath / "archive", SystemPrincipal) map { cacheFolder ⇒
            // Get any sessions that were previously cached
            val previous = readUncachedSessions(cacheFolder)
            cacheFolder close()
            val writer = CfsFiles.newBufferedWriter(chhome.getPath / sessionCacheName,
                                                    MIME_TYPE("application/json"), StandardOpenOption.CREATE,
                                                    StandardOpenOption.TRUNCATE_EXISTING,
                                                    StandardOpenOption.WRITE)(() ⇒ SystemPrincipal)
            try {
                Serialization.write(previous, writer)
                previous
            }
            finally {
                writer close()
            }
        } openOr Nil
    }

    override def messageHandler : PartialFunction[Any, Unit] = {
        case GetSessionCacheRequest(chhome) ⇒
            val result = tryo(readUpdatedSessionCache(chhome))
            reply(result map GetSessionCacheResponse openOr result)
        case UpdateChoiceletState(chstate) ⇒
            update(chstate)
    }
}

object ChoiceletCacheManager {
    import Helpers.TimeSpan

    sealed case class GetSessionCacheRequest(chhome : CfsFolder)
    sealed case class GetSessionCacheResponse(array : Array[ChoiceletState])
    sealed case class UpdateChoiceletState(chstate : ChoiceletState)

    private val choiceletActors : mutable.Map[Long, ChoiceletCacheManager] = mutable.Map()
    private val updateCheckerRunning = new AtomicBoolean(false)

    val sessionCacheName = "session-cache.json"

    def clearCacheEntry(choiceletId : Long) : Unit = choiceletActors.synchronized {
        choiceletActors get choiceletId foreach { actor ⇒
            if (actor.lastUpdateAge >= 0) {
                actor.write()
            }
            choiceletActors remove choiceletId
        }
    }

    def getSessionCache(chhome : CfsFolder) : Box[Array[ChoiceletState]] = {
        val cmanager = choiceletActors.synchronized {
            choiceletActors get chhome.getResourceId.id match {
                case Some(actor) ⇒ actor
                case None ⇒
                    val newActor = new ChoiceletCacheManager(chhome.getPath)
                    choiceletActors put (chhome.getResourceId.id, newActor)
                    newActor
            }
        }
        cmanager !! GetSessionCacheRequest(chhome) match {
            case Full(GetSessionCacheResponse(array)) ⇒ Full(array)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from GetSessionCacheRequest")
        }
    }

    def updateChoiceletState(chstate : ChoiceletState) : Unit = {
        val cmanager = choiceletActors.synchronized {
            choiceletActors get chstate.client match {
                case some @ Some(_) ⇒ some
                case None ⇒
                    val chpath = Cfs open (CfsVFileId(chstate.client), SystemPrincipal, CfsOpenOptions.Default) match {
                        case Full(chhome : CfsFolder) ⇒
                            try {
                                Some(chhome.getPath)
                            }
                            finally {
                                chhome close ()
                            }
                        case Full(other) ⇒
                            other close ()
                            None
                        case _ ⇒ None
                    }
                    chpath map { homePath ⇒
                        val newActor = new ChoiceletCacheManager(homePath)
                        choiceletActors put(chstate.client, newActor)
                        newActor
                    }
            }
        }
        cmanager foreach (_ ! UpdateChoiceletState(chstate))
        if (updateCheckerRunning.compareAndSet(false, true)) {
            Schedule(() ⇒ updateChecker(chstate.client, millis), TimeSpan(TimeHelpers.seconds(40)))
        }
    }

    def updateChecker(choiceletId : Long, startTime : Long) : Unit = {
        choiceletActors.synchronized {
            choiceletActors get choiceletId match {
                case Some(actor) ⇒
                    val updateAge = actor.lastUpdateAge
                    if ((millis - startTime > 60000) || (updateAge > 30000)) {
                        actor.write()
                        updateCheckerRunning.set(false)
                    }
                    else if (updateAge > 0) {
                        Schedule(() ⇒ updateChecker(choiceletId, startTime), TimeSpan(TimeHelpers.seconds(40)))
                    }
                    else updateCheckerRunning.set(false)
                case None ⇒ updateCheckerRunning.set(false)
            }
        }
    }
}

/**
  * Definitive information about a Choicelet.
  */
trait ChoiceletDefinition {
    /**
      * @return the Choicelet's unique id, which corresponds to the resource id of its home folder
      */
    def id : Long

    /**
      * @return the Choicelet model name, which is usually the same for all versions of a Choicelet
      */
    def name : String

    /**
      * @return the Choicelet version string, which in combination with the model name uniquely
      *         identifies the Choicelet
      */
    def version : String

    /** Return Choicelet information as a map **/
    def asMap : Map[String, Any] = Map("id" → id, "name" → name, "version" → version)
}

/**
  * Query specification for a Choicelet. The unique id of a Choicelet may be specified using
  * either 'id' or 'client'. When 'client' is specified, 'id' is ignored. In the context of
  * a ChoiceletSummarySpec, 'id' is the state id of a Choicelet session when it is present.
  *
  * @param id the unique id of the Choicelet
  * @param name the Choicelet model name
  * @param version the Choicelet version string
  * @param client the unique id of the Choicelet
  */
case class ChoiceletSpec(id : Option[Long], name : Option[String], version : Option[String], client : Option[Long]) {

    def matchChoicelet(choicelet : ChoiceletDefinition) : Boolean = {
        matchId(choicelet) && matchName(choicelet) && matchVersion(choicelet)
    }

    def matchId(choicelet : ChoiceletDefinition) : Boolean = {
        client match {
            case Some(c) ⇒ c == choicelet.id
            case None ⇒ id.fold (true) (_ == choicelet.id)
        }

    }

    def matchName(choicelet : ChoiceletDefinition) : Boolean = {
        name.fold (true) (_ == choicelet.name)
    }

    def matchVersion(choicelet : ChoiceletDefinition) : Boolean = {
        version.fold (true) (_ == choicelet.version)
    }

    def isSameChoicelet_?(choicelet : ChoiceletDefinition) : Boolean = {
        matchChoicelet(choicelet)
    }
}

case class ChoiceletSummarySpec(name : Option[String], version : Option[String], client : Option[Long],
                                id : Option[Long], closed : Option[Boolean], deluxe : Option[Boolean],
                                group : Option[String], gid : Option[Long],
                                from : Option[Long], until : Option[Long]) {

    private var _gid : Option[Long] = None

    def getChoiceletSpec : ChoiceletSpec = ChoiceletSpec(None, name, version, client)

    def matchChoicelet(choicelet : ChoiceletDefinition) : Boolean = {
        matchId(choicelet) && matchName(choicelet) && matchVersion(choicelet)
    }

    def matchName(choicelet : ChoiceletDefinition) : Boolean = {
        name.fold (true) (_ == choicelet.name)
    }

    def matchVersion(choicelet : ChoiceletDefinition) : Boolean = {
        version.fold (true) (_ == choicelet.version)
    }

    def matchId(choicelet : ChoiceletDefinition) : Boolean = {
        client.fold (true) (_ == choicelet.id)
    }

    def matchState(chstate : ChoiceletState, principal : Principal) : Boolean = {
        matchStateId(chstate) && matchClosed(chstate) && matchDeluxe(chstate) &&
            matchFromTime(chstate) && matchUntilTime(chstate) && matchGroup(chstate, principal)
    }

    def matchStateId(chstate : ChoiceletState) : Boolean = {
        id.fold (true) (_ == chstate.id)
    }

    def matchClosed(chstate : ChoiceletState) : Boolean = {
        closed.fold (true) (_ == chstate.closed)
    }

    def matchDeluxe(chstate : ChoiceletState) : Boolean = {
        val hasDeluxe = chstate.hasDeluxe getOrElse false
        deluxe.fold (true) (_ == hasDeluxe)
    }

    def matchGroup(chstate : ChoiceletState, principal: Principal) : Boolean = {
        // If there is a gid in the query, any group path in the query is ignored.
        // Otherwise try for a _gid previously derived from group in the query.
        // Otherwise, if there is a group path in the query, try to translate it
        // to a group id.
        val queryGroupId = gid.orElse(_gid) orElse {
            group foreach { gpath ⇒
                val gidbox = Cfs.withExistingFile(gpath, principal, CfsOpenOptions.Default) {
                    case ginfo : GroupInfo ⇒
                        Full(ginfo.getResourceId.id)
                }
                // If the translation failed, the resulting group id is -1, which will never
                // match a state group.
                _gid = Some(gidbox openOr -1L)
            }
            // If _gid is None at this point, it means neither gid nor group are in the query.
            _gid
        }
        // If queryGroupId is None at this point, the state group always matches.
        // If the state group id cannot be retrieved, it is assumed not to match.
        queryGroupId.fold (true) { groupId ⇒
            // The userPath is considered most definitive, otherwise the original user
            val upath = chstate.userPath getOrElse chstate.user
            Cfs.withExistingFile(upath, principal, CfsOpenOptions.Default) {
                case uinfo : UserInfo ⇒
                    val gidList = uinfo.getGroupList map {
                        case (_, ginfo) ⇒
                            val gid = ginfo.getResourceId.id
                            ginfo close ()
                            gid
                    }
                    Full(gidList contains groupId)
            } openOr false
        }
    }

    def matchFromTime(chstate : ChoiceletState) : Boolean = {
        from.fold(true) (chstate.stime >= _)
    }

    def matchUntilTime(chstate : ChoiceletState) : Boolean = {
        until.fold(true) (chstate.stime <= _)
    }
}

/**
  * Information about a single Choicelet session.
  *
  * @param id unique id of a Choicelet session, aka "state id"
  * @param sid unique id of the HTTP session
  * @param user the user path at the start of session
  * @param userid the unique id of the Choicelet user
  * @param jsessionid the HTTP session cookie value
  * @param client the unique id of the Choicelet, aka "client id", "choicelet id"
  * @param closed true if the session was closed, usually meaning the Choicelet was finished
  * @param stime the server start time for the session
  * @param tstamp the client start time for the session
  * @param json the initial model parameters, JSON-encoded
  * @param altid the id of this session on Choice if it was migrated from Choice
  * @param endClient the client end of session timestamp, from the last logged event
  * @param userPath the full user path, retrieved from the HTTP session if "user" is guest.
  *                 Otherwise likely the same as "user"
  */
case class ChoiceletState(id : Long, sid : Long, user : String, userid : Long,
                          jsessionid : String, client : Long, closed : Boolean,
                          stime : Long, tstamp : Long, json : String, altid : Option[Long],
                          endClient : Option[Long] = None,         // filled when state is cached
                          userPath : Option[String] = None,        // filled when state is cached
                          hasDeluxe : Option[Boolean] = None)      // set true when Deluxe file is written

/**
  * Handle for the home folder of a particular Choicelet version.
  */
sealed trait ChoiceletHomeHandle extends ChoiceletDefinition {

    protected var _closeWaiters : List[ChoiceletHomeHandle ⇒ Unit] = Nil

    /** Close this handle **/
    def close() : Unit = {
        _closeWaiters foreach (f ⇒ f(this))
        _closeWaiters = Nil
    }

    /** Specify a function to be called when this handle is being closed **/
    def onClose(f : ChoiceletHomeHandle ⇒ Unit) : ChoiceletHomeHandle = {
        _closeWaiters = f :: _closeWaiters
        this
    }

    /** Get a stream of session state handles for one or all sessions of this Choicelet **/
    def getChoiceletStateStream(id : Option[Long]) : Stream[ChoiceletState]

    /**
      * Create a new session for this Choicelet.
      *
      * @param sfolder the session folder for the HTTP session
      * @param tstamp the client's timestamp of the start of the session
      * @param args Choicelet model arguments, JSON-encoded
      * @param closed a flag indicating whether this session has been closed
      * @param stime the server's timestamp of the start of the session
      * @param altid the id of this session under Choice, if it was migrated from Choice
      * @return
      */
    def createChoiceletState(sfolder : SessionFolder, tstamp : Long, args : String,
                             closed : Boolean = false, stime : Long = millis,
                             altid : Option[Long] = None) : Box[(CfsAbsolutePath, ChoiceletState)]
}
