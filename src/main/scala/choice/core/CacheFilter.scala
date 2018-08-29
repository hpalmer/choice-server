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
  * Implement caching of plain files which are open to anonymous read access.
  *
  * @author Howard Palmer
  */
package choice.core

import java.io.IOException
import java.nio.file.{Files, Path}
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import choice.fs.ChoiceData
import choice.model.ResourceId
import net.liftweb.common.Logger
import net.liftweb.util.Helpers

import scala.collection.mutable

case class CacheFileInfo(resid : ResourceId, seqnum : Long, contentType : String, length : Int, mtime : Long) {
    def getNotModifiedHeaders : List[(String, String)] = List(
        ("Cache-Control", "public,max-age=31356000"),
        ("Last-Modified", Helpers.toInternetDate(mtime))
    )

    def getResponseHeaders : List[(String, String)] = List(
        ("Content-Type", contentType),
        ("Cache-Control", "public,no-cache,max-age=1800"),
        ("Date", Helpers.nowAsInternetDate),
        ("Last-Modified", Helpers.toInternetDate(mtime))
    )

    def asMap : Map[String, Any] = Map(
        "resid" → resid.id,
        "seqnum" → seqnum,
        "contentType" → contentType,
        "length" → length,
        "mtime" → mtime
    )
}

class CacheFilter extends Filter {
    import choice.core.CacheFilter.Log

    def respond(inPath : Path, out : ServletOutputStream, path : String) : Unit = {
        try {
            Files.copy(inPath, out)
        }
        catch {
            case ex : IOException ⇒
                Log.error(s"I/O exception serving $path: ${ex.getMessage}")
        }
        finally {
            out flush ()
        }
    }

    override def init(config : FilterConfig) : Unit = {
        Log.info("init")
    }

    override def doFilter(req : ServletRequest, rsp : ServletResponse, chain : FilterChain) : Unit = {
        val handled = (req, rsp) match {
            case (httpreq : HttpServletRequest, httprsp : HttpServletResponse) if httpreq.getMethod == "GET" ⇒
                val ctxpath = httpreq.getContextPath
                val uri = httpreq.getRequestURI
                if (uri startsWith ctxpath) {
                    val myuri = uri substring ctxpath.length
                    CacheFilter lookup myuri match {
                        case Some(cfinfo @ CacheFileInfo(_, seqNum, _, length, _)) ⇒
                            val ifmsTime = httpreq.getDateHeader("If-Modified-Since")
                            if ((ifmsTime != -1L) && ((cfinfo.mtime / 1000) <= (ifmsTime / 1000))) {
                                cfinfo.getNotModifiedHeaders foreach {
                                    case (name, value) ⇒ httprsp setHeader (name, value)
                                }
                                httprsp setStatus HttpServletResponse.SC_NOT_MODIFIED
                            }
                            else {
                                cfinfo.getResponseHeaders foreach {
                                    case (name, value) ⇒ httprsp setHeader(name, value)
                                }
                                httprsp setContentLength length
                                httprsp setStatus HttpServletResponse.SC_OK
                                if (seqNum > 0) {
                                    val rspout = httprsp.getOutputStream
                                    respond(ChoiceData getPath seqNum, rspout, myuri)
                                }
                            }
                            true
                        case None ⇒
                            Log.debug(s"lookup failed for $myuri (ctxpath = $ctxpath)")
                            false
                    }
                }
                else {
                    Log.error(s"$uri does not start with $ctxpath")
                    false
                }
            case _ ⇒ false
        }
        if (!handled) {
            chain doFilter (req, rsp)
        }
    }

    override def destroy() : Unit = {
        Log.info("destroy")
    }

//    object cacheEntry extends TransientRequestVar[Option[CacheFileInfo]](None)
//
//    override def isDefinedAt(req: Req): Boolean = {
//        req.get_? && {
//            CacheFilter lookup req.uri match {
//                case entry @ Some(_) ⇒
//                    cacheEntry(entry)
//                    true
//                case None ⇒ false
//            }
//        }
//    }
//
//    override def apply(req: Req): () => Box[LiftResponse] = {
//        val rsp = cacheEntry.get match {
//            case Some(CacheFileInfo(resid, seqNum, contentType, length)) ⇒
//                ChoiceData getInputStream seqNum match {
//                    case Full(in) ⇒
//                        Full(StreamingResponse(in, () ⇒ { in close () }, length, ("Content-Length", length.toString) ::
//                            ("Content-Type", contentType) :: Nil, Nil, 200))
//                    case e : EmptyBox ⇒
//                        CacheFilter remove resid
//                        Full(TemporaryRedirectResponse(req.uri, req, req.cookies : _*))
//                }
//            case None ⇒ Full(TemporaryRedirectResponse(req.uri, req, req.cookies : _*))
//        }
//        () ⇒ rsp
//    }
}

object CacheFilter {
    val Log = Logger("choice.core.CacheFilter")

    val cache : mutable.HashMap[String, Long] = collection.mutable.HashMap[String, Long]()
    val idcache : mutable.HashMap[Long, CacheFileInfo] = collection.mutable.HashMap[Long, CacheFileInfo]()

    def lookup(uri : String) : Option[CacheFileInfo] = synchronized {
        cache get uri match {
          case Some(id) ⇒ idcache get id
          case None ⇒ None
        }
    }

    def add(uri : String, resid : ResourceId, seqNum : Long,
            contentType : String, length : Int, mtime : Long) : CacheFileInfo = {
        synchronized {
            Log.debug(s"adding $uri")
            cache put(uri, resid.id)
            // Check whether the CacheFileInfo entry is present and has the same sequence number
            // and modification time.
            idcache get resid.id match {
              case Some(cfinfo) if seqNum == cfinfo.seqnum && mtime == cfinfo.mtime ⇒ cfinfo
              case _ ⇒
                  val cfinfo = CacheFileInfo(resid, seqNum, contentType, length, mtime)
                  idcache put (resid.id, cfinfo)
                  cfinfo
            }
        }
    }

    def remove(resid : ResourceId) : Unit = {
        synchronized {
            idcache remove resid match {
                case Some(_) ⇒
                    var klist = List[String]()
                    for ((k, v) ← cache) {
                        if (v == resid.id) klist = k :: klist
                    }
                    for (k ← klist) {
                        Log.info(s"removing $k")
                        cache remove k
                    }
                case None ⇒
            }
        }
    }

    def remove(uri : String) : Boolean = {
        synchronized {
            cache get uri match {
                case Some(id) ⇒
                    remove(ResourceId(id))
                    true
                case None ⇒ false
            }
        }
    }

    def clear() : Unit = {
        synchronized {
            cache clear ()
            idcache clear ()
        }
    }

    def dump() : (List[(String, Long)], List[(Long, Map[String, Any])]) = {
        synchronized {
            (cache.toList, idcache.toList.map(pair ⇒ (pair._1, pair._2.asMap)))
        }
    }
}