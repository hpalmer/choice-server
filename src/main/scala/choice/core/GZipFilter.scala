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
  * @author Howard Palmer
  */
package choice.core

import java.io._
import java.util.zip.GZIPOutputStream

import bootstrap.liftweb.Boot
import javax.servlet._
import javax.servlet.http._
import net.liftweb.common.Logger

import scala.util.matching.Regex

/**
 * This filter will GZip the server response, if the browser supports it and a number of
 * conditions on the response itself are met. Currently these conditions are:
 *
 *  - Content-Type must match a configured MIME type
 *  - content length must exceed a configured threshold size in bytes
 *
 * Created by Hep on 8/25/2014.
 */
class GZipFilter extends Filter {
    import choice.core.GZipFilter._

    lazy val Log : Logger = Boot.getDefaultLogger

    protected var threshold : Int = DEFAULT_THRESHOLD
    protected var mimetypes : List[String] = DEFAULT_MIMETYPES


    override def init(config : FilterConfig) : Unit = {
        val th = config.getInitParameter("threshold")
        if (th != null) {
            try {
                th.toInt match {
                    case n if n <= 0 ⇒ threshold = 0
                    case n if n > MAX_THRESHOLD ⇒
                        Log.error(s"(GZipFilter) $n exceeds maximum threshold $MAX_THRESHOLD (maybe you don't want this plugin?)")
                        threshold = MAX_THRESHOLD
                    case n ⇒ threshold = n
                }
            }
            catch {
                case _ : NumberFormatException ⇒
                    Log.error(s"(GZipFilter) invalid threshold value, '$th', ignored")
            }
        }
        val mt = config.getInitParameter("mimetypes")
        if (mt != null) {
            val mtypes = mt.split(',').toList
            for (t ← mtypes) {
                val tt = t.trim
                tt.charAt(0) match {
                    case '+' ⇒
                        val rtt = tt.substring(1).toLowerCase
                        if (!(mimetypes contains rtt)) mimetypes = rtt :: mimetypes
                    case '-' ⇒
                        val rtt = tt.substring(1).toLowerCase
                        mimetypes = mimetypes.takeWhile(_ != rtt)
                    case '^' ⇒
                        val rtt = tt.substring(1).toLowerCase
                        mimetypes = rtt :: Nil
                    case _ ⇒
                        val rtt = tt.toLowerCase
                        if (!(mimetypes contains rtt)) mimetypes = rtt :: mimetypes
                }
            }
        }
    }

    override def doFilter(req : ServletRequest, rsp : ServletResponse, chain : FilterChain) : Unit = {
        (req, rsp) match {
            case (httpreq : HttpServletRequest, httprsp : HttpServletResponse) ⇒
                val gzipOk = getHeaderValues(httpreq, "accept-encoding") exists { hdrval ⇒
                    val (enc, q) = valueWithQ(hdrval)
                    enc.equalsIgnoreCase("gzip") && (q > 0.0)
                }
                if (gzipOk) {
                    val wrappedResponse = new GZipResponseWrapper(httprsp, threshold, mimetypes)
                    chain doFilter (httpreq, wrappedResponse)
                    wrappedResponse close ()
                }
                else chain doFilter (httpreq, httprsp)
            case _ ⇒ chain doFilter (req, rsp)
        }
    }

    protected def getHeaderValues(req : HttpServletRequest, name : String) : Iterator[String] = {
//        import scala.collection.JavaConversions.enumerationAsScalaIterator
        import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter
        val outer = req.getHeaders(name)
        for (hdr ← outer.asScala; v ← hdr.trim split ',') yield v.trim
    }

    protected def valueWithQ(s : String) : (String, Double) = {
        val parts = s split ';'
        if (parts.length == 1) (parts(0), 1.0)
        else if (parts.length == 2) {
            val v = parts(0).trim
            val QPattern = """q\s*=\s*((?:1(?:\.0{1,3})?)|(?:0(?:\.\d{1,3})?))""".r
            parts(1).trim match {
                case QPattern(q) ⇒
                    try {
                        (v, q.toDouble)
                    }
                    catch {
                        case _ : NumberFormatException ⇒
                            Log.error(s"(GZipFilter) NumberFormatException on '$q'")
                            (v, 0.0)
                    }

            }
        }
        else {
            Log.error(s"(GZipFilter) too many ';' in '$s'")
            (s, 0.0)
        }
    }

    override def destroy() : Unit = {

    }
}

object GZipFilter {

    /** Default threshold on content length at which compression will be done */
    val DEFAULT_THRESHOLD = 4096

    /** Maximum supported threshold value */
    val MAX_THRESHOLD : Int = 256*1024

    val DEFAULT_MIMETYPES = List(
        "text/html",
        "application/javascript", "text/javascript", "application/x-javascript",
        "text/css",
        "text/plain",
        "application/xml"
    )

    val NO_TRANSFORM : Regex = "(?i)(?:(?:.*)[\\s,])?no-transform(?:[\\s,](?:.*))?".r
}

class GZipResponseWrapper(response : HttpServletResponse,
                          threshold : Int, mimetypes : List[String]) extends HttpServletResponseWrapper(response) {

    import choice.core.GZipFilter.NO_TRANSFORM

    protected var gzipOutputStream : Option[GZipResponseStream] = None
    protected var printWriter : Option[PrintWriter] = None
    protected var contentLength : Option[Int] = None

    /**
     * Intercept attempts to set the content length, as the compressed stream generally
     * will be smaller.
     *
     * @param len content length
     */
    override def setContentLength(len : Int) : Unit = {
        contentLength = Some(len)
    }

    override def getOutputStream : ServletOutputStream = {
        if (printWriter.isDefined) {
            throw new IllegalStateException("getWriter() has already been called!")
        }
        gzipOutputStream getOrElse {
            val gzipout = new GZipResponseStream()
            gzipOutputStream = Some(gzipout)
            gzipout
        }
    }

    override def getWriter : PrintWriter = {
        printWriter getOrElse {
            if (gzipOutputStream.isDefined) {
                throw new IllegalStateException("getOutputStream() has already been called!")
            }
            val gzipout = new GZipResponseStream()
            val writer = new PrintWriter(new OutputStreamWriter(gzipout, "UTF-8"))
            printWriter = Some(writer)
            writer
        }
    }

    override def flushBuffer() : Unit = {
        //PrintWriter.flush() does not throw exception
        printWriter foreach (_.flush())

        val exception1 = try {
            gzipOutputStream foreach (_.flush())
            None
        }
        catch {
            case io : IOException ⇒ Some(io)
        }

        val exception2 = try {
            super.flushBuffer()
            None
        }
        catch {
            case io : IOException ⇒ Some(io)
        }

        exception1 foreach { io ⇒ throw io }
        exception2 foreach { io ⇒ throw io }
    }

    def close() : Unit = {
        printWriter foreach (_.flush())
        gzipOutputStream match {
            case None ⇒ response.getOutputStream close ()
            case Some(gzipout) ⇒
                if (!gzipout.isClosed) {
                    gzipout flush()
                    gzipout close()
                }
        }
    }

    /**
     * Check all conditions that prevent gzip encoding, except for the output length
     * being under the threshold.
     *
     * @return a boolean indicating whether gzip encoding should be suppressed
     */
    def noGzip : Boolean = {
        checkContentRange || checkCacheControl || checkContentType
    }

    def noGzipEarly : Boolean = {
        (contentLength match {
            case Some(clen) ⇒
                if (clen >= threshold) {
                    // If the content-type hasn't been set yet, gzip still possible.
                    // If it has been set, it should be one of the configured MIME types
                    response.getContentType match {
                        case null ⇒ false
                        case _ ⇒ checkContentType
                    }
                }
                else true           // content-length is less than the threshold
            case None ⇒ false
        }) || checkContentRange || checkCacheControl
    }

    /**
     * Check whether a Content-Type setting prevents gzip encoding. Only MIME types
     * which are specifically configured (possibly by DEFAULT_MIMETYPES) are subject
     * to gzip encoding.
     *
     * @return a boolean indicating whether gzip encoding should be suppressed
     */
    def checkContentType : Boolean = {
        response.getContentType match {
            case null ⇒ true
            case ctype ⇒
                val ct = (ctype split ';')(0)
                !(mimetypes contains ct.toLowerCase)
        }
    }

    /**
     * Check whether a Content-Range setting prevents gzip encoding.
     *
     * @return a boolean indicating whether gzip encoding should be suppressed
     */
    def checkContentRange : Boolean = response.getHeader("content-range") != null

    /**
     * Check whether a cache-control setting prevents gzip encoding.
     *
     * @return a boolean indicating whether gzip encoding should be suppressed
     */
    def checkCacheControl : Boolean = {
        response.getHeader("cache-control") match {
            case null ⇒ false
            case cc ⇒ NO_TRANSFORM.unapplySeq(cc).isDefined
        }
    }

    /**
     * Inner class for servlet output stream for GZipped response. The decision whether
     * to gzip is deferred until the output length threshold is reached, by buffering
     * up to the threshold number of bytes internally. There is also a possibility that
     * the noGzip condition will be set in the outer class before the threshold is reached.
     * In that case any buffered output and any subsequent output will be forwarded to
     * the original output stream.
     */
    class GZipResponseStream extends ServletOutputStream {
        protected var closed = false

        /**
         * This is initially where uncompressed data is stored, before the threshold
         * is reached. When compression is enabled, it will be replaced with a buffer
         * which contains the gzip output.
         */
        protected var byteOutputStream : ByteArrayOutputStream = _

        /**
         * The gzip output stream is not created until the threshold has been reached.
         */
        protected var gzipOutputStream : Option[GZIPOutputStream] = None

        /**
         * The original output stream is obtained when the threshold is reached, if
         * gzip encoding is suppressed. Otherwise it is not obtained until this
         * stream is closed.
         */
        protected var originalOutput : Option[ServletOutputStream] = None

        init()

        def init() : Unit = {
            val nogzip = noGzipEarly
            if (nogzip) {
                contentLength foreach { clen ⇒ response.setContentLength(clen) }
                originalOutput = Some(response.getOutputStream)
            }
            else {
                byteOutputStream = new ByteArrayOutputStream(threshold)
            }
        }

        override def write(b : Int) : Unit = {
            if (closed) {
                throw new IOException("Cannot write to a closed output stream")
            }
            originalOutput match {
                case None ⇒ gzipOutputStream match {
                    case None ⇒
                        byteOutputStream write b
                        if (byteOutputStream.size() >= threshold) {
                            getThresholdOutput
                        }
                    case Some(gzip) ⇒ gzip write b
                }
                case Some(orig) ⇒ orig write b
            }
        }

        override def write(b : Array[Byte]) : Unit = write(b, 0, b.length)

        override def write(b : Array[Byte], off : Int, len : Int) : Unit = {
            if (closed) {
                throw new IOException("Cannot write to a closed output stream")
            }
            originalOutput match {
                case None ⇒ gzipOutputStream match {
                    case None ⇒
                        if (byteOutputStream.size() + len >= threshold) {
                            getThresholdOutput write (b, off, len)
                        }
                        else byteOutputStream write (b, off, len)
                    case Some(gzip) ⇒ gzip write (b, off, len)
                }
                case Some(orig) ⇒ orig write (b, off, len)
            }
        }

        override def flush() : Unit = {
            if (closed) {
                throw new IOException("Cannot flush a closed output stream")
            }
            originalOutput match {
                case None ⇒ gzipOutputStream match {
                    case None ⇒ byteOutputStream.flush()  // Probably does nothing
                    case Some(gzip) ⇒ gzip.flush()
                }
                case Some(orig) ⇒ orig.flush()
            }
        }

        override def close() : Unit = {
            if (closed) {
                throw new IOException("This output stream has already been closed")
            }

            originalOutput match {
                case None ⇒ gzipOutputStream match {
                    case None ⇒
                        // Total size was under the threshold
                        val clen = contentLength getOrElse byteOutputStream.size()
                        response.setContentLength(clen)
                        writeBufferedBytes close ()
                    case Some(gzip) ⇒
                        gzip finish ()
                        response setContentLength byteOutputStream.size()
                        response addHeader ("Content-Encoding", "gzip")
                        response addHeader ("vary", "Accept-Encoding")
                        writeBufferedBytes close ()
                }
                case Some(orig) ⇒
                    orig flush ()
                    orig close ()
            }

            closed = true
        }

        def isClosed : Boolean = closed

        /**
         * Write any buffered bytes to the original response output stream.
         */
        def writeBufferedBytes : ServletOutputStream = {
            val origout = originalOutput getOrElse {
                val out = response.getOutputStream
                originalOutput = Some(out)
                out
            }
            if (byteOutputStream.size() > 0) {
                origout write byteOutputStream.toByteArray
                origout flush ()
            }
            origout
        }

        /**
         * This is called when the threshold has been (or is being) reached. A final check is
         * made to see if gzip encoding is ok. If not, the buffered bytes are written to the
         * original response output stream, and subsequent output will be directed there.
         * Otherwise a gzip stream is created, the buffered bytes are piped into it, and
         * subsequent output will be directed there. Once gzipping has been started, it
         * continues until all the gzip output is buffered, so that the content-length can
         * be set.
         *
         * @return either the original response output stream or a gzip output stream
         */
        def getThresholdOutput : OutputStream = {
            if (noGzip) {
                // Write out any buffered output and switch to the original response output stream
                contentLength foreach { clen ⇒
                    response.setContentLength(clen)
                }
                val origout = response.getOutputStream
                originalOutput = Some(origout)
                if (byteOutputStream.size() > 0) {
                    val buffer = byteOutputStream.toByteArray
                    origout write buffer
                }
                origout
            }
            else {
                val gzippedbytes = new ByteArrayOutputStream()
                val gzipout = new GZIPOutputStream(gzippedbytes)
                gzipOutputStream = Some(gzipout)
                if (byteOutputStream.size() > 0) {
                    val buffer = byteOutputStream.toByteArray
                    gzipout write buffer
                }
                // The byteOutputStream is now the gzip output buffer
                byteOutputStream = gzippedbytes
                gzipout
            }
        }
    }
}

