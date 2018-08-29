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
  * @author Howard Palmer
  */
package choice.lib

import choice.access._
import choice.lib.JsonHelpers._
import net.liftweb.common._
import net.liftweb.http.{LiftResponse, Req}
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JString
import net.liftweb.util.Helpers._

/**
 * Common interface of all Ajax request types
 */
trait AjaxRequest { self ⇒
    def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse]

    def processRequest(req : Req) : Box[LiftResponse] = {
        SessionClient.withSessionState (requireLogin = false) { sclient ⇒
            getResponse (req, sclient, sclient.getPrincipal)
        } match {
            case full @ Full(_) ⇒ full
            case e : EmptyBox ⇒ FailureResponse(e)
        }
    }
}

trait AjaxRequestExtractor[+R <: AjaxRequest] {
    def getReq(req : Req) : Box[R]
}

abstract class AjaxApiRequest(api : String, op : String) extends AjaxRequest

case class AjaxApiRequestExtractor[+R <: AjaxRequest : Manifest](api : String, op : String)
    extends AjaxRequestExtractor[R] {
    implicit val formats = DefaultFormats

    def getReq(req : Req) : Box[R] = {
        req.json flatMap { jo ⇒
            tryo(jo.extract[R]) ?~! s"bad parameters for $api operation $op"
        }
    }
}

class AjaxRequestTable(optable : Map[String, AjaxApiRequestExtractor[AjaxRequest]]) {

    def getReq(req : Req) : Box[AjaxRequest] = {
        if (req.json_?) {
            req.json match {
                case Full(jo) ⇒
                    jo \ "op" match {
                        case JString(op) ⇒ optable get op match {
                            case Some(extractor) ⇒ extractor.getReq(req)
                            case None ⇒ Failure(s"no such operation: $op")
                        }
                        case _ ⇒ Failure("invalid or missing op argument")
                    }
                case e: EmptyBox ⇒ e
            }
        }
        else Empty
    }
}
