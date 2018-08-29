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
  * Servlet filter to log incoming POST requests.
  *
  * @author Howard Palmer
  * Created by Hep on 4/4/2017.
  */
package choice.core

import java.io._
import java.nio.charset.StandardCharsets
import javax.servlet.http.{HttpServletRequest, HttpServletRequestWrapper}
import javax.servlet.{ServletInputStream, _}

import net.liftweb.common.Logger

import scala.collection.mutable

private class RequestWrapper @throws[IOException]
        (val request : HttpServletRequest) extends HttpServletRequestWrapper(request) {

    private lazy val body : Array[Byte] = {

        val contentLength = request.getContentLength
        val inputStream = request.getInputStream
        if (inputStream != null) {
            if (contentLength >= 0) {
                def reader(buffer : Array[Byte], pos : Int) : Array[Byte] = {
                    val bytesRead = inputStream.read(buffer, pos, buffer.length - pos)
                    if (bytesRead <= 0) {
                        buffer.slice(0, pos)
                    }
                    else {
                        val newpos = pos + bytesRead
                        if (newpos < buffer.length) reader(buffer, newpos)
                        else buffer
                    }
                }
                reader(new Array[Byte](contentLength), 0)
            }
            else {
                def growingReader(buffer : Array[Byte], pos : Int) : Array[Byte] = {
                    val bytesRead = inputStream.read(buffer, pos, buffer.length - pos)
                    if (bytesRead <= 0) {
                        buffer.slice(0, pos)
                    }
                    else {
                        val newpos = pos + bytesRead
                        if (newpos < buffer.length) {
                            growingReader(buffer, newpos)
                        }
                        else {
                            val newBuffer = new Array[Byte](buffer.length + 1024)
                            System.arraycopy(buffer, 0, newBuffer, 0, newpos)
                            growingReader(newBuffer, newpos)
                        }
                    }
                }
                growingReader(new Array[Byte](1024), 0)
            }
        }
        else new Array[Byte](0)
    }

    private lazy val bodyAsString = new String(body, StandardCharsets.UTF_8)

    @throws[IOException]
    override def getInputStream : ServletInputStream = {
        val byteArrayInputStream = new ByteArrayInputStream(body)
        new ServletInputStream() {
            @throws[IOException]
            override def read : Int = byteArrayInputStream.read

            override def close() : Unit = request.getInputStream.close()
        }
    }

    @throws[IOException]
    override def getReader = new BufferedReader(new InputStreamReader(this.getInputStream))

    def getBody : Array[Byte] = body

    def getBodyAsString : String = bodyAsString
}

class LogFilter extends Filter {
    import LogFilter.Log

    override def init(filterConfig : FilterConfig) : Unit = {
        Log.info(s"init: Tomcat log folder is ${LogFilter.tomcatLogFolder}")
    }

    override def destroy() : Unit = {
        Log.info("destroy")
        LogFilter.closeAll()
    }

    override def doFilter(request : ServletRequest, response : ServletResponse, chain : FilterChain) : Unit = {
        request match {
            case httpRequest : HttpServletRequest if httpRequest.getMethod == "POST" ⇒
                // Cookies can be null
                Option(httpRequest.getCookies)
                    .flatMap(_.find(_.getName == Startup.jSessionIdName).map(_.getValue)) match {
                    case Some(sessionId) ⇒
                        LogFilter.getPrintWriter(sessionId) match {
                            case Some(pw) ⇒
                                // Log this POST
                                val wrappedRequest = new RequestWrapper(httpRequest)
                                val query = httpRequest.getQueryString
                                val url = httpRequest.getRequestURI + (if (query != null) s"?$query" else "")
                                pw.println(raw"""{ "type" : "post",
                                                |  "time" : ${System.currentTimeMillis()},
                                                |  "url" : "$url",
                                                |  "post" : ${wrappedRequest.getBodyAsString}
                                                |},""".stripMargin)
                                chain doFilter (wrappedRequest, response)
                            case None ⇒
                                // Session is not being logged
                                chain doFilter (request, response)
                        }
                    case None ⇒
                        // No session, don't log
                        chain doFilter (request, response)
                }
            case _ ⇒
                // Not an HTTP POST request
                chain doFilter (request, response)
        }
    }
}

object LogFilter {
    val Log = Logger("choice.core.LogFilter")

    lazy val tomcatLogFolder : String = System.getProperty("catalina.base") + "/logs"

    private val sessionLogFiles = mutable.Map[String, PrintWriter]()

    def enableLogging(session : String) : Unit = {
        Log.info(s"enable $session")
        val pw = sessionLogFiles get session match {
            case None ⇒
                val newPrintWriter = new PrintWriter(s"$tomcatLogFolder/LF-$session.json")
                sessionLogFiles.synchronized {
                    sessionLogFiles.put(session, newPrintWriter)
                    newPrintWriter
                }
            case Some(existingPrintWriter) ⇒ existingPrintWriter
        }
        pw.println(
            raw"""[ { "type" : "init",
                 |  "session" : "$session",
                 |  "time" : ${System.currentTimeMillis()}
                 |},""".stripMargin)
    }

    def disableLogging(session : Option[String]) : Unit = {
        session match {
            case Some(id) ⇒
                Log.info(s"disable $id")
                sessionLogFiles.synchronized {
                    sessionLogFiles get id match {
                        case Some(pw) ⇒
                            pw.println(raw"""{ "type" : "close",
                                            |  "session" : "$id",
                                            |  "time" : ${System.currentTimeMillis()}
                                            |} ]""".stripMargin)

                            pw flush ()
                            pw close ()
                        case None ⇒
                    }
                }
            case None ⇒
                Log.info("disable all")
                closeAll()
        }
    }

    def getPrintWriter(sessionId : String) : Option[PrintWriter] = sessionLogFiles.synchronized {
        sessionLogFiles.get(sessionId)
    }

    def closeAll() : Unit = {
        sessionLogFiles.synchronized {
            for ((id, logWriter) ← sessionLogFiles) {
                logWriter.println(raw"""{ "type" : "close",
                                       |  "session" : "$id",
                                       |  "time" : ${System.currentTimeMillis()}
                                       |} ]""".stripMargin)
                logWriter flush ()
                logWriter close ()
            }
        }
    }
}