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

import java.nio.file.StandardOpenOption

import choice.access.CfsPrincipal
import choice.fs._
import net.liftweb.common.{Full, _}
import net.liftweb.http.{LiftResponse, Req}
import net.liftweb.json.JsonAST.{JArray, JObject, JString, _}
import net.liftweb.json.{DefaultFormats, JsonAST}
import net.liftweb.util.Helpers._

/**
 * A session activity for logging data to a specified output file.
 */
class SessionLogger(out : CfsBufferedWriter) extends SessionActivity {

    def dispatch(req : Req) : Box[LiftResponse] = SessionLogger getResponse (req, this)

    def close() : Unit = tryo {
        out flush ()
        out close ()
    }

    def put(data : String) : Box[Boolean] = {
        tryo {
            out write data
            true
        }
    }

    def flush() : Unit = {
        tryo (out flush ())
    }
}

object SessionLogger extends SessionActivityDispatcher[SessionLogger] with SessionActivityBuilder[SessionLogger] {
    val activityType : String = "logger"
    implicit val formats : DefaultFormats.type = DefaultFormats

    def optable : Map[String, SessionActivityExtractor[SessionLogger, SessionActivityRequest[SessionLogger]]] = Map (
        "put" → SessionActivityExtractor[SessionLogger, PutLogRequest](SessionLogger, "put")
    )

    def buildActivity(req : Req, sclient : SessionClient) : Box[SessionLogger] = {
        implicit val principalImpl : () ⇒ CfsPrincipal = () ⇒ sclient.getPrincipal
        req.json flatMap (_.extractOpt[OpenLogRequest]) match {
            case Full(oreq) ⇒
                val path = Cfs.withValidPath(oreq.path) {
                    case abspath : CfsAbsolutePath ⇒ Full(abspath)
                    case cfspath ⇒ Full(CfsAbsolutePath(CfsRootRoot, cfspath.allParts))
                }
                path flatMap { abspath ⇒
                    val mimetype = oreq.mimetype getOrElse "text/plain"
                    val append = oreq.append getOrElse true
                    val options = List(
                        StandardOpenOption.WRITE,
                        StandardOpenOption.CREATE,
                        MIME_TYPE(mimetype),
                        if (append) StandardOpenOption.APPEND else StandardOpenOption.TRUNCATE_EXISTING
                    )
                    tryo(CfsFiles.newBufferedWriter(abspath, options : _*)) map (out ⇒ new SessionLogger(out))
                }
            case e : EmptyBox ⇒ e
        }
    }
}

case class OpenLogRequest(path : String, append : Option[Boolean], mimetype : Option[String])

case class PutLogRequest(data : JValue) extends SessionActivityRequest[SessionLogger] {
    def getResponse(req : Req, obj : SessionLogger) : Box[LiftResponse] = {
        def helper(jv : JValue, level : Int) : Unit = {
            jv match {
                case JString(s) ⇒
                    // Add a newline if there isn't one already
                    val ss = if (s.endsWith("\n")) s else s"$s\n"
                    obj put ss
                case JArray(a) ⇒
                    if (level == 0) a foreach (elem ⇒ helper(elem, 1))
                    else obj put s"${JsonAST.compactRender(jv)}\n"
                case jo @ JObject(_) ⇒ obj put s"${JsonAST.compactRender(jo)}\n"
                case JBool(b) ⇒ obj put s"${b.toString}\n"
                case JInt(i) ⇒ obj put s"${i.toString()}\n"
                case JDouble(d) ⇒ obj put s"${d.toString}\n"
                case JNull ⇒ obj put "null\n"
                case other ⇒ obj put s"${other.toString}\n"
            }
        }
        helper (data, 0)
        JsonHelpers.MapResponse(Map("status" → 1))
    }
}
