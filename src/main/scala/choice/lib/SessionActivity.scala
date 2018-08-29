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
  * Implement stateful activities which can be associated with an HTTP session.
  *
  * @author Howard Palmer
  */
package choice.lib

import java.util.concurrent.atomic.AtomicLong

import net.liftweb.common.{Box, EmptyBox, Failure, Full}
import net.liftweb.http.rest.RestContinuation
import net.liftweb.http.{JsonResponse, LiftResponse, Req}
import net.liftweb.json
import net.liftweb.json.JsonAST.JString
import net.liftweb.json._
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._

import scala.collection.mutable

trait SessionActivityMap {
    protected val _activities : mutable.Map[String, SessionActivity] = mutable.Map[String, SessionActivity]()

    def activities : Map[String, SessionActivity] = _activities.toMap

    def getActivity(id : String) : Option[SessionActivity] = _activities get id

    def existsActivity(id : String) : Boolean = getActivity(id).isDefined

    def putActivity(id : String, activity : SessionActivity) : Boolean = {
        _activities get id match {
            case Some(_) ⇒ false
            case None ⇒
                _activities put (id, activity)
                true
        }
    }

    def withActivity[T](id : String)(pf : PartialFunction[SessionActivity, Box[T]]) : Box[T] = {
        getActivity(id) match {
            case Some(activity) ⇒
                (pf orElse {
                    case _ ⇒ Failure(s"$id is not the expected type of activity")
                } : PartialFunction[SessionActivity, Box[T]])(activity)
            case None ⇒ Failure(s"no $id activity")
        }
    }

    def closeActivity(id : String) : Unit = {
        _activities get id match {
            case Some(act) ⇒
                act close ()
                _activities remove id
            case None ⇒
        }
    }
}

/**
 * Base class for a session activity. A session activity contains per-session
 * state which is manipulated via Ajax operations.
 */
abstract class SessionActivity {
    def dispatch(req : Req) : Box[LiftResponse]
    def close() : Unit
}

trait SessionActivityDispatcher[T <: SessionActivity] {
    def activityType : String
    def optable : Map[String, SessionActivityExtractor[T, SessionActivityRequest[T]]]
    def requestExtractor : SessionActivityTable[T] = new SessionActivityTable(optable)
    def getResponse(req : Req, act : T) : Box[LiftResponse] = {
        requestExtractor getReq req flatMap (_.getResponse(req, act)) match {
            case full @ Full(_) ⇒ full
            case e : EmptyBox ⇒ JsonHelpers.FailureResponse(e)
        }
    }
}

/**
 * Incoming Ajax requests are decoded into various parameter container classes
 * which implement SessionActivityRequest. Each such class implements a
 * particular operation on the associated SessionActivity.
 *
 */
trait SessionActivityRequest[T <: SessionActivity] {
    def getResponse(req : Req, obj : T) : Box[LiftResponse]
}

trait SessionRequestExtractor[T <: SessionActivity] {
    def getReq(req : Req) : Box[SessionActivityRequest[T]]
}

/**
 * Each type of SessionActivity has an associated builder that can create a
 * new instance of that SessionActivity type from information in an Ajax
 * request.
 *
 */
trait SessionActivityBuilder[T <: SessionActivity] {
    def buildActivity(req : Req, sclient : SessionClient) : Box[T]
}

object SessionActivityBuilder {
    def build(activityType : String, req : Req, sclient : SessionClient) : Box[SessionActivity] = {
        activityType match {
            case "environ" ⇒ SessionEnvironment buildActivity (req, sclient)
            case "logger" ⇒ SessionLogger buildActivity (req, sclient)
            case _ ⇒ Failure(s"unknown activity type: $activityType")
        }
    }
}

case class SessionActivityExtractor[T <: SessionActivity,
                                    +S <: SessionActivityRequest[T] : Manifest](dispatcher : SessionActivityDispatcher[T],
                                                                               op : String)
    extends SessionRequestExtractor[T] {
    implicit val formats : DefaultFormats.type = DefaultFormats

    def getReq(req : Req) : Box[SessionActivityRequest[T]] = {
        (req.json flatMap (_.extractOpt[S])) ?~! s"bad parameters for ${dispatcher.activityType} operation $op"
    }
}

class SessionActivityTable[T <: SessionActivity](optable : Map[String, SessionActivityExtractor[T, SessionActivityRequest[T]]]) {
    def getReq(req : Req) : Box[SessionActivityRequest[T]] = {
        req.json match {
            case Full(jo) ⇒
                jo \ "op" match {
                    case JString(op) ⇒ optable get op match {
                        case Some(extractor) ⇒ extractor.getReq(req)
                        case None ⇒ Failure(s"no such operation: $op")
                    }
                    case _ ⇒ Failure("invalid or missing op argument")
                }
            case e : EmptyBox ⇒ e
        }
    }
}

/**
 * This represents a listener on an environment, which is notified whenever the
 * value of a variable changes (or all the variable values change). The listener
 * is identified by a unique id. It can specify a particular environment operation
 * to match, and/or a particular variable name. If the variable name ends with "*",
 * it matches any variable name starting with the string preceding the "*".
 *
 * Internally this tracks whether the listener has a poll operation outstanding,
 * and keeps a list of changes to the environment that should be returned on the
 * next poll.
 *
 * @param id the unique id of this listener
 * @param op an optional environment operation to match
 * @param name an optional variable name to match
 */
case class EnvironListener(id : Long, op : Option[String], name : Option[String]) {
    import scala.language.postfixOps
    import json.Extraction.decompose

    implicit val formats : DefaultFormats.type = DefaultFormats

    private var pollFn : Option[(⇒ LiftResponse) ⇒ Unit] = None
    private var changeList : List[Map[String, Any]] = Nil
    private var lastResponseTime = millis

    def poll() : LiftResponse = {
        RestContinuation.async { rf ⇒
            Schedule.schedule(() ⇒ response(), 30 seconds)
            EnvironListener.this.synchronized {
                // If another poll is already pending, terminate it
                respond()
                // Set this poll pending
                pollFn = Some(rf)
                // If changes are already pending, complete the new poll now
                if (changeList != Nil) {
                    respond()
                }
            }
        }
    }

    /**
     * Notify this listener of a change to the environment.
     *
     * @param opch the environment operation causing the change
     * @param namech the name of the variable changed, if singular
     * @param valuech the new value of the variable, if singular
     */
    def notifyChange(opch : String, namech : Option[String], valuech : Option[String]) : Unit = {
        val opmatch = op forall (_ == opch)
        val nmatch = {
            name match {
                case Some(ns) ⇒
                    if (ns.endsWith("*")) {
                        namech forall (_.startsWith(ns.substring(0, ns.length - 1)))
                    }
                    else namech forall (_ == ns)
                case None ⇒ true
            }
        }
        // If this change matches our criteria, add an entry to the change list
        if (opmatch && nmatch) {
            val entry : Map[String, Any] = Map("op" → opch, "name" → namech, "value" → valuech)
            EnvironListener.this.synchronized {
                changeList = entry :: changeList
                // If there's a poll pending, complete it
                respond()
            }
        }
    }

    /**
     * Schedule an immediate response to any pending poll.
     */
    def respond() : Unit = {
        pollFn foreach { _ ⇒ Schedule.schedule(() ⇒ response(), 0 seconds) }
    }

    def response() : Unit = {
        EnvironListener.this.synchronized {
            pollFn match {
                case Some(pf) ⇒
                    val now = millis
                    val elapsed = now - lastResponseTime
                    // If there are changes, or at least 30 seconds have elapsed,
                    // respond to any pending poll.
                    if ((changeList != Nil) || (elapsed >= 30000)) {
                        pollFn = None
                        val changes = {
                            // Convert changeList to JSON, and also reverse it, so that the entries
                            // are in ascending temporal order.
                            val jvlist = (changeList foldLeft (Nil: List[JValue])) { (list, elem) ⇒
                                decompose(elem) :: list
                            }
                            changeList = Nil
                            JArray(jvlist)
                        }
                        lastResponseTime = millis
                        pf(JsonResponse(changes))
                    }
                    else {
                        // No changes to report yet. Wait until at least 30 seconds
                        // have elapsed since the last response.
                        Schedule.schedule(() ⇒ response(), TimeSpan(30000 - elapsed))
                    }
                case None ⇒
                    // No poll pending
            }
        }
    }
}

class SessionEnvironment extends SessionActivity {
    import json.Extraction.decompose

    implicit val formats : DefaultFormats.type = DefaultFormats

    val _environ : mutable.Map[String, List[String]] = collection.mutable.Map[String, List[String]]()

    val _listenId = new AtomicLong(0L)
    var _listenList : List[EnvironListener] = Nil

    def getDispatcher : SessionActivityDispatcher[SessionEnvironment] = SessionEnvironment

    def dispatch(req : Req) : Box[LiftResponse] = SessionEnvironment.getResponse(req, this)

    def get(name : String) : Option[String] = _environ get name flatMap (_.headOption)

    def set(name : String, value : Option[String]) : Option[String] = {
        val result = _environ get name flatMap (_.headOption)
        value match {
            case Some(s) ⇒ _environ put(name, s :: Nil)
            case None ⇒ _environ remove name
        }
        notifyChange("set", Some(name), value)
        result
    }

    def push(name : String, value : String) : Option[String] = {
        val prevlist = _environ put (name, value :: (_environ getOrElse (name, Nil)))
        val result = prevlist flatMap (_.headOption)
        notifyChange("push", Some(name), Some(value))
        result
    }

    def pop(name : String) : Option[String] = {
        _environ get name match {
            case Some(Nil) ⇒ None
            case Some(_ :: tail) ⇒
                _environ put (name, tail)
                val result = tail.headOption
                notifyChange("pop", Some(name), result)
                result
            case None ⇒ None
        }
    }

    def getAll : Map[String, List[String]] = _environ.toMap

    def setAll(env : Map[String, List[String]]) : Map[String, List[String]] = {
        _environ.clear()
        _environ ++= env
        _environ.toMap
    }

    def init(env : Map[String, String]) : Unit = {
        _environ.clear()
        for ((name, value) ← env) _environ put (name, value :: Nil)
        notifyChange("init", None, None)
    }

    def listen(op : Option[String], name : Option[String]) : Long = {
        val listener = EnvironListener(_listenId.incrementAndGet, op, name)
        SessionEnvironment.this.synchronized {
            _listenList = listener :: _listenList
        }
        listener.id
    }

    def poll(listener : Long) : LiftResponse = {
        val elistener = SessionEnvironment.this.synchronized { _listenList find (_.id == listener) }
        // The call to EnvironListener's poll() typically throws an exception, caught
        // by Lift, which sets up a delayed response. However, if there is no listener
        // with the specified id, we need to return an error response.
        elistener map (_.poll()) getOrElse
            JsonResponse(decompose(Map("status" → -1, "msg" → s"no listener with id $listener")))
    }

    def notifyChange(opch : String, namech : Option[String], valuech : Option[String]) : Unit = {
        if (_listenList != Nil) {
            SessionEnvironment.this.synchronized {
                _listenList foreach { _.notifyChange(opch, namech, valuech) }
            }
        }
    }

    def unlisten(listener : Long) : Boolean = {
        SessionEnvironment.this.synchronized {
            _listenList find (_.id == listener) match {
                case Some(envlistener) ⇒
                    // Terminate any outstanding poll on the listener
                    Schedule.schedule(() ⇒ envlistener.response(), TimeSpan(0L))
                    // Remove the listener from the list
                    _listenList = _listenList filterNot (_.id == listener)
                    true
                case None ⇒ false
            }
        }
    }

    def close() : Unit = {
        _environ.clear()
        SessionEnvironment.this.synchronized {
            _listenList foreach (_.respond())
            _listenList = Nil
        }
    }
}

object SessionEnvironment extends SessionActivityDispatcher[SessionEnvironment]
                                  with SessionActivityBuilder[SessionEnvironment] {
    val activityType = "environ"

    val optable : Map[String, SessionActivityExtractor[SessionEnvironment, SessionActivityRequest[SessionEnvironment]]] = Map (
        "init" → SessionActivityExtractor[SessionEnvironment, InitEnvRequest](SessionEnvironment, "init"),
        "get" → SessionActivityExtractor[SessionEnvironment, GetEnvRequest](SessionEnvironment, "get"),
        "set" → SessionActivityExtractor[SessionEnvironment, SetEnvRequest](SessionEnvironment, "set"),
        "push" → SessionActivityExtractor[SessionEnvironment, PushEnvRequest](SessionEnvironment, "push"),
        "pop" → SessionActivityExtractor[SessionEnvironment, PopEnvRequest](SessionEnvironment, "pop"),
        "getall" → SessionActivityExtractor[SessionEnvironment, GetAllEnvRequest](SessionEnvironment, "getall"),
        "setall" → SessionActivityExtractor[SessionEnvironment, SetAllEnvRequest](SessionEnvironment, "setall"),
        "listen" → SessionActivityExtractor[SessionEnvironment, ListenEnvRequest](SessionEnvironment, "listen"),
        "unlisten" → SessionActivityExtractor[SessionEnvironment, UnlistenEnvRequest](SessionEnvironment, "unlisten"),
        "poll" → SessionActivityExtractor[SessionEnvironment, PollEnvRequest](SessionEnvironment, "poll")
    )

    def buildActivity(req : Req, sclient : SessionClient) : Box[SessionEnvironment] = Full(new SessionEnvironment)
}

case class GetEnvRequest(name : String) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val resultMap = obj get name match {
            case Some(s) ⇒ Map(name → s)
            case None ⇒ Map.empty[String, String]
        }
        JsonHelpers.MapResponse(Map("status" → 1, "result" → resultMap))
    }
}

case class SetEnvRequest(name : String, value : Option[String]) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val resultMap = obj set (name, value) match {
            case Some(s) ⇒ Map(name → s)
            case None ⇒ Map.empty[String, String]
        }
        JsonHelpers.MapResponse(Map("status" → 1, "result" → resultMap))
    }
}

case class PushEnvRequest(name : String, value : String) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val resultMap = obj push (name, value) match {
            case Some(s) ⇒ Map(name → s)
            case None ⇒ Map.empty[String, String]
        }
        JsonHelpers.MapResponse(Map("status" → 1, "result" → resultMap))
    }
}

case class PopEnvRequest(name : String) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val resultMap = obj pop name match {
            case Some(s) ⇒ Map(name → s)
            case None ⇒ Map.empty[String, String]
        }
        JsonHelpers.MapResponse(Map("status" → 1, "result" → resultMap))
    }
}

case class GetAllEnvRequest() extends SessionActivityRequest[SessionEnvironment] {
    override def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        JsonHelpers.MapResponse(Map("status" → 1, "result" → obj.getAll))
    }
}

case class SetAllEnvRequest(env : Map[String, List[String]]) extends SessionActivityRequest[SessionEnvironment] {
    override def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        JsonHelpers.MapResponse(Map("status" → 1, "result" → obj.setAll(env)))
    }
}

case class InitEnvRequest(env : JObject) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val rawmap = env.values
        val initMap = rawmap map { pair ⇒
            val (name, v) = pair
            (name, v.toString)
        }
        obj init initMap
        JsonHelpers.MapResponse(Map("status" → 1))
    }
}

case class ListenEnvRequest(oplisten : Option[String], name : Option[String])
                            extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val id = obj listen (oplisten, name)
        JsonHelpers.MapResponse(Map("status" → 1, "listener" → id))
    }
}

case class UnlistenEnvRequest(listener : Long) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        val result = obj unlisten listener
        JsonHelpers.MapResponse(Map("status" → 1, "result" → result))
    }
}

case class PollEnvRequest(listener : Long) extends SessionActivityRequest[SessionEnvironment] {
    def getResponse(req : Req, obj : SessionEnvironment) : Box[LiftResponse] = {
        Full(obj poll listener)
    }
}
