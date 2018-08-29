/**
  * Copyright © 2013-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * Routes are used to map from URL namespace to the Choice filesystem namespace.
  * A route essentially consumes a portion of the incoming URL and maps it to an
  * object in the filesystem, which could be a folder, an ordinary file, or a
  * special file known as a Component. An ordinary file is simply served to the
  * client, with any remaining URL components being ignored (or passed as PATH_INFO).
  * A folder becomes the context for interpreting any remaining URL components as
  * a relative path in the filesystem. A Component is a special file which can
  * contain its own Routes. When a Route maps to a Component, the Component's
  * Routes are applied to any remaining URL components, and this process
  * continues until all URL components are consumed.
  *
  * @author Howard Palmer
  */
package choice.core

import java.util.Locale
import javax.servlet.http.HttpServletResponse

import choice.access._
import choice.actor.SessionManager
import choice.fs._
import choice.lib.SessionClient
import choice.model.GlobalConfig
import net.liftweb.common.{Full, _}
import net.liftweb.http.{InMemoryResponse, StreamingResponse, _}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers.tryo

import scala.xml.NodeSeq

object RequestContextPath extends RequestVar[Box[String]](Empty)

/**
 * This is a trait of a target which can produce a LiftResponse.
 */
trait Responsible {
    def getLiftResponse(principal : Principal) : Box[LiftResponse]
}

/**
 * Base class for different kinds of results from resolving a URL.
 */
sealed abstract class RouteResult

/**
 * No matching route found.
 */
case object NoRoute extends RouteResult with Responsible with LiftResponse with HeaderDefaults {
    override def toResponse: BasicResponse = {
        val url = S.request map { req ⇒
            val path = req.path
            req.contextPath concat
                path.wholePath.mkString(if (path.absolute) "/" else "", "/", if (path.endSlash) "/" else "")
        } openOr "<missing URL>"
        val content = ResponseTemplates.getTemplateString("404.html", List(("url", url)))
        InMemoryResponse(content.getBytes("UTF-8"),
                         "Content-Type" → "text/html; charset=utf-8" :: headers, cookies, 404)
    }

    def getLiftResponse(principal : Principal) : Box[LiftResponse] = {
        Full(toResponse)
    }
}

sealed case class RouteRedirect(to : String) extends RouteResult with Responsible {
    def getLiftResponse(principal : Principal) : Box[LiftResponse] = {
        val query = S.queryString
        val url = query map (to + "?" + _) openOr to
        Full(RedirectResponse(url, S.responseCookies : _*))
    }
}

sealed case class RouteNeedsLogin(group : Box[GroupInfo], urlIndex : Int, path : CfsPath)
    extends RouteResult with Responsible {
    def getLiftResponse(principal : Principal) : Box[LiftResponse] = {
        S.param("redirect") match {
            case Empty ⇒
                val doRedirect = {
                    Cfs open (path, SystemPrincipal, CfsOpenOptions.Default) match {
                        case Full(folder : CfsFolder) ⇒
                            folder close ()
                            true
                        case Full(plain : CfsPlain) ⇒
                            val ishtml = plain.getMimeType.dmap(false)(_ == "text/html")
                            plain close ()
                            ishtml
                        case Full(other) ⇒
                            other close ()
                            false
                        case _ : EmptyBox ⇒ true
                    }
                }
                if (doRedirect) {
                    val ginfo = group openOr (GlobalConfig getDefaultGroup SystemPrincipal)
                    SessionClient.withSessionState(requireLogin = false) { sclient ⇒
                        Full(sclient setLoginGroup ginfo.getPath.toString)
                    }
                    val guestPage = ginfo.getGuestPage openOr "/index"
                    val redirect = S.uriAndQueryString map (("redirect", _))
                    val uri = Helpers.appendParams(guestPage, redirect.toList)
                    ginfo close()
                    Full(RedirectResponse(uri))
                }
                else {
                    group foreach (_ close ())
                    Full(ForbiddenResponse("authentication required"))
                }
            case _ ⇒
                group foreach (_ close ())
                Full(ForbiddenResponse("login redirect failed"))
        }
    }
}

sealed case class RoutePlainFile(protected val file : CfsPlain, component : Component) extends RouteResult with Responsible {

    def getLiftResponse(principal : Principal) : Box[LiftResponse] = {
        val result : Box[LiftResponse] = file info () flatMap { info ⇒
            S.request flatMap { req ⇒
                val mimetype = info.mimeType
                if (mimetype == "text/html" || mimetype == "application/xhtml+xml") {
                    component.getData foreach { desc ⇒
                        // The component may contain a group, which becomes the default login group.
                        // If the user has no SessionClient yet, save the group in the HTTP session.
                        // Otherwise store it in the SessionClient.
                        val logingroup = desc.group getOrElse Startup.DefaultUsersPath
                        SessionManager getSessionClient (create = false) match {
                            case Some(sclient) ⇒ sclient setLoginGroup logingroup
                            case None ⇒
                                S.session flatMap (_.httpSession) foreach { httpSession ⇒
                                    httpSession.setAttribute("LOGINGROUP", logingroup)
                                }
                        }
                    }
                    tryo(CfsFiles.newInputStream(file)) flatMap { in ⇒
                        LiftRules.externalTemplateResolver.request.set(() ⇒ RoutePlainFile.templateResolver(component))
                        // Note: the htmlParser closes the InputStream, in. Twice actually.
                        S.htmlProperties.htmlParser(in) flatMap { nodeseq ⇒
                            val stripped = Templates.checkForContentId(nodeseq)
                            S.session flatMap { liftSession ⇒
                                liftSession.processTemplate(Full(stripped), req, req.path, 200)
                            }
                        }
                    }
                }
                else {
                    tryo(CfsFiles.newInputStream(file)) flatMap { in ⇒
                        // Called when the StreamingResponse below completes
                        def endResponse() : Unit = {
                            in.close()
                        }

                        // Check for conditional GET
                        val ifmsTime = req.ifModifiedSince match {
                            case Full(d) ⇒ d.getTime
                            case _ ⇒ -1L
                        }
                        if ((ifmsTime != -1) && ((info.mtime / 1000) <= (ifmsTime / 1000))) {
                            val headers = List(
                                ("Cache-Control", "public,no-cache,max-age=1800"),
                                ("Last-Modified", Helpers.toInternetDate(info.mtime))
                            )
                            in close ()
                            Full(InMemoryResponse(Array(), headers, Nil, HttpServletResponse.SC_NOT_MODIFIED))
                        }
                        else {
                            val headers = {
                                // If a guest can read this file, cache it
                                if (RoutePlainFile.guestCanReadFile_?(file)) {
                                    val cfinfo = CacheFilter.add(S.uri,
                                        file.getResourceId,
                                        file.getVnode.getResource.getSeqnum,
                                        info.mimeType, info.size.toInt, info.mtime)
                                    cfinfo.getResponseHeaders
                                }
                                else {
                                    // Not readable by guest, so don't cache it. But allow the browser
                                    // to cache.
                                    val d = Helpers.nowAsInternetDate
                                    List(
                                        ("Content-Length", info.size.toString),
                                        ("Content-Type", info.mimeType),
                                        ("Last-Modified", Helpers.toInternetDate(info.mtime)),
                                        ("Cache-Control", "private,no-cache"),
                                        ("Expires", d),
                                        ("Date", d)
                                    )
                                }
                            }
                            Full(StreamingResponse(in, () ⇒ endResponse(), info.size, headers, Nil, HttpServletResponse.SC_OK))
                        }
                    }
                }
            }
        }
        component close ()
        result match {
            case full @ Full(_) ⇒ full
            case e : EmptyBox ⇒ Full(RouteFailureResponse(e))
        }
    }

}

object RoutePlainFile {
    import choice.core.Route.Log

    val liftSuffixes = List("html", "htm", "xml")

    def templateResolver(component : Component) : PartialFunction[(Locale, List[String]), Box[NodeSeq]] = {
        case (_ : Locale, list: List[String]) ⇒
            SessionClient.withSessionState(requireLogin = false) { ss ⇒
                component resolve(list, 0, list, ss.getPrincipal) match {
                    case RoutePlainFile(tempfile, comp) ⇒
                        val result = tryo(CfsFiles.newInputStream(tempfile)) flatMap { in ⇒
                            val xml = S.htmlProperties.htmlParser(in)
                            in close()
                            xml
                        }
                        if (!(comp eq component)) comp close()
                        result
                    case _ ⇒
                        Log.error( s"""failed to resolve template: ${list.mkString("/")}""")
                        Empty
                }
            }
    }

    def guestCanReadFile_?(file: CfsPlain): Boolean = {
        (file.getPrincipalId == GuestPrincipal.getPrincipalId) || {
            Cfs open(file.getPath, GuestPrincipal, CfsOpenOptions.Default) match {
                case Full(plain: CfsPlain) ⇒
                    val result = (CfsPlain canGetInputStream plain) { () ⇒ Full(true)}
                    plain close()
                    result openOr false
                case Full(other) ⇒
                    other close()
                    false // shouldn't ever happen since file is CfsPlain
                case _ ⇒ false
            }
        }
    }
}

sealed case class RouteFailure(e : EmptyBox) extends RouteResult with Responsible {
    def getLiftResponse(principal : Principal) : Box[LiftResponse] = Full(RouteFailureResponse(e))
}

case class RouteFailureResponse(f : EmptyBox) extends LiftResponse with HeaderDefaults {
    import choice.core.Route.Log
    Log.error(f)

    def toResponse : InMemoryResponse = {
        val msg = f match {
            case Empty ⇒ "result is Empty"
            case f : Failure ⇒ f.msg
        }
        InMemoryResponse(msg.getBytes, headers, cookies, 500)
    }
}

case class RoutePath(path : List[String], absolute : Boolean)

/**
 * Base class for different kinds of routes, which principally vary in the
 * particular type of RouteTarget they produce.
 *
 */
case class Route(urlParts : List[String], path : RoutePath) {

    /**
     * Check whether the urlParts of this Route match a prefix of the specified
     * parts sequence. If all the urlParts match,
     *
     * @param parts a list of URL parts to match against this route
     * @return None if not all urlParts of this route match a prefix of 'parts'.
     *         Otherwise the suffix of 'parts' that are not matched.
     */
    def checkMatch(parts : List[String]) : Option[List[String]] = {
        if (parts.startsWith(urlParts)) Some(parts.drop(urlParts.length)) else None
    }

    def length : Int = urlParts.length

    /**
      * This route path may be absolute or relative. If it's absolute, just
      * return the absolute path. Otherwise it is taken to be relative to
      * the parent of the component which contains the route, and that
      * absolute path is constructed based on the absolute path of the
      * component.
      *
      * @param componentPath the absolute path of the component file
      * @return the absolute path of the target of this route
      */
    def absolutePath(componentPath : CfsAbsolutePath) : CfsAbsolutePath = {
        val relpath = CfsRelativePath(path.path)
        if (path.absolute) CfsRootPath resolve relpath
        else componentPath.getParent resolve relpath
    }
}

object Route {
    val Log = Logger("choice.core.Route")
}
