/**
  * Copyright © 2014-2017 The Board of Trustees of The Leland Stanford Junior University.
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

import net.liftweb.common.{Full, _}
import net.liftweb.http.{LiftResponse, LiftRules, PostRequest, Req}
import net.liftweb.json.DefaultFormats

/**
 * Handle Ajax operations on session activities.
 */
object SessionOps extends Logger {

    def init() : Unit = {
        LiftRules.dispatch.prepend {
            // register the group operations handler
            case r @ Req(_, _, PostRequest) if r.param("api") == Full("session") ⇒ () ⇒ handleOp(r)
        }
    }

    case class SessionRequest(id : String, op : String, kind : Option[String])

    def handleOp(req : Req) : Box[LiftResponse] = {
        implicit val formats : DefaultFormats.type = DefaultFormats

        req.json match {
            case Full(jv) ⇒
                jv.extractOpt[SessionRequest] match {
                    case Some(sreq) ⇒
                        SessionClient.withSessionState(requireLogin = false) { ss ⇒
                            sreq.op match {
                                case "create" ⇒
                                    ss closeActivity sreq.id
                                    sreq.kind match {
                                        case Some(actType) ⇒
                                            SessionActivityBuilder build (actType, req, ss) match {
                                                case Full(act) ⇒
                                                    ss putActivity (sreq.id, act)
                                                    JsonHelpers.MapResponse(Map("status" → 1))
                                                case e : EmptyBox ⇒ JsonHelpers.FailureResponse(e)
                                            }
                                        case None ⇒
                                            JsonHelpers.SimpleResponse(-1, "missing the kind of activity to create")
                                    }
                                case "exists" ⇒
                                    val (result, isKind) = ss getActivity sreq.id match {
                                        case Some(act) ⇒ sreq.kind match {
                                            case Some("environ") ⇒ (true, act.isInstanceOf[SessionEnvironment])
                                            case Some("logger") ⇒ (true, act.isInstanceOf[SessionLogger])
                                            case Some(_) ⇒ (true, false)
                                            case None ⇒ (true, true)
                                        }
                                        case None ⇒ (false, false)
                                    }
                                    JsonHelpers.MapResponse(Map("status" → 1, "result" → result, "isKind" → isKind))
                                case "close" ⇒
                                    ss closeActivity sreq.id
                                    JsonHelpers.MapResponse(Map("status" → 1))
                                case _ ⇒
                                    ss getActivity sreq.id match {
                                        case Some(act) ⇒ act dispatch req
                                        case None ⇒ JsonHelpers.SimpleResponse(-1, s"${sreq.id} does not exist")
                                    }
                            }
                        }
                    case None ⇒ JsonHelpers.SimpleResponse(-1, "missing parameters for session request")
                }
            case e : EmptyBox ⇒ JsonHelpers.FailureResponse(e)
        }
    }
}
