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

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json.Extraction._
import net.liftweb.json.Implicits._
import net.liftweb.json._

/**
 * Helper functions for encoding/decoding JSON
 * @author Howard Palmer
 *
 */
object JsonHelpers {
    implicit val formats : DefaultFormats.type = DefaultFormats

    def SimpleResponse(status : Int, msg : String) : Box[LiftResponse] = {
        Full(JsonResponse(statusAsJson(status, msg)))
    }

    def statusAsJson(status : Int, msg : String) : JObject = {
        JObject(JField("status", status) ::
            JField("msg", msg) :: Nil)
    }

    def StatusResponse(status : Int) = Full(JsonResponse(JObject(JField("status", status) :: Nil)))

    def NilResponse = Full(JsonResponse(JObject(Nil)))

    def SuccessResponse : Box[LiftResponse] = SimpleResponse(1, "ok")

    def ImpossibleResponse : Box[LiftResponse] = SimpleResponse(-1, "not supposed to be possible")

    def InvalidOperation : Box[LiftResponse] = {
        SimpleResponse(-1, "invalid operation")
    }

    def FailureResponse(e : EmptyBox) : Box[LiftResponse] = {
        val msg = e match {
            case Empty ⇒ "result is Empty"
            case f : Failure ⇒ f.toString
        }
        SimpleResponse(-1, msg)
    }

    def MissingParameter(op : String) : Box[LiftResponse] = {
        SimpleResponse(-1, "missing parameter(s) for " + op)
    }

    def MapResponse(map : Map[String, Any]) : Box[LiftResponse] = {
        val mapWithStatus = if (map.get("status").isDefined) map else map + ("status" → 1)
        Full(JsonResponse(decompose(mapWithStatus)))
    }

    def ArrayResponse(seq : Seq[AnyRef]) : Box[LiftResponse] = {
        Full(JsonResponse(decompose(seq)))
    }
}
