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
  * Created by Hep on 4/3/2013.
  */
package choice.lib

import _root_.net.liftweb._
import common._
import util._
import Helpers._
import java.io.InputStream
import java.util.zip._
import java.io.BufferedInputStream

import org.apache.tika.config.TikaConfig
import org.apache.tika.detect.Detector
import org.apache.tika.metadata.Metadata
import org.apache.tika.metadata.TikaMetadataKeys

case class ArchiveMember(dirpath : String,
        			 name : String,
        			 modtime : Long,
        			 isdir : Boolean,
        			 private val instream : InputStream) {
    
    private val markstream = if (instream.markSupported) instream else new BufferedInputStream(instream)
    
    private val _mimeType = MimeTypeDetector.getMimeType(markstream, Some(name))
    
    def fullPath : String = if (dirpath == "") name else dirpath + "/" + name
    
    /**
     * Return the input stream for this archive member. If the MIME type of the member
     * is desired, call getMimeType *before* reading the input stream, because reading
     * the input stream may interfere with MIME type detection.
     */
    def getInputStream : InputStream = markstream
    
    /**
     * Return the MIME type of this archive member. Call this before reading the
     * member's input stream.
     */
    def getMimeType : String = _mimeType
    
    /**
     * Is the archive member a directory?
     */
    def isDirectory_? : Boolean = isdir
}

object MimeTypeDetector {
    val Log = Logger("choice.lib.MimeTypeDetector")

    val DEFAULT_MIME_TYPE = "application/octet-stream"

    val config : TikaConfig = TikaConfig.getDefaultConfig
    val detector : Detector = config.getDetector

    def getMimeType(in : InputStream, filename : Option[String]) : String = {
        if (!in.markSupported) Log.error("MimeTypeDetector: called with stream without mark support")
        val metadata = new Metadata
        filename match {
            case Some(fn) ⇒ metadata.set(TikaMetadataKeys.RESOURCE_NAME_KEY, fn)
            case None ⇒
        }
        val result = tryo(detector.detect(in, metadata)) match {
            case Full(mediatype) ⇒
                val mimetype = mediatype.toString
                if (mimetype contains "tika") {
                    Log.warn("tika returned MIME type: " + mimetype)
                    DEFAULT_MIME_TYPE
                }
                else {
                    // Work-around for Tika-1141 Returns text/html if JavaScript file contains "<html>"
                    if (mimetype == "text/html" && filename.fold(false)(_.endsWith(".js"))) "application/javascript"
                    // Firefox (at least) doesn't like audio/vorbis
                    else if (mimetype == "audio/vorbis") "audio/ogg"
                    else mimetype
                }
            case e : EmptyBox ⇒
                Log.error("tika failed to detect MIME type for " + filename, e)
                DEFAULT_MIME_TYPE
        }
        Log.info("found mimeType: " + result)
        result
    }
}

abstract class ArchiveFile[T <: ZipEntry](in : InputStream) extends Iterator[ArchiveMember] {
    import ZipFile.Log

    protected val myiterator : Iterator[T]
    
    override def hasNext : Boolean = myiterator.hasNext
    
    override def next() : ArchiveMember = {
        val jentry = myiterator.next()
        tryo {
            val (dirpath, name) = {
                val path = jentry.getName
                val i = path.lastIndexOf("/")
                if (i > 0) (path.substring(0, i), path.substring(i + 1))
                else ("", path)
            }
            val modtime = jentry.getTime
            // isdir never seems to be true, but just in case
            val isdir = jentry.isDirectory || (name == "")
            if (isdir) Log.info("zip directory: (" + dirpath + "," + name + ")")
            val member = ArchiveMember(dirpath, name, modtime, isdir, in)
            Log.info("ZipFile processed " + member.fullPath)
            member
        } match {
            case Full(member) ⇒ member
            case e : EmptyBox ⇒
                Log.error(e ?~! "error processing zip file")
                null
        }
    }
}

class ZipFile(zin : ZipInputStream) extends ArchiveFile[ZipEntry](zin) {

    override val myiterator : Iterator[ZipEntry] = Iterator continually zin.getNextEntry takeWhile (_ != null)
}

object ZipFile {
    val Log = Logger("choice.lib.ZipFile")

	def apply(zin : ZipInputStream) = new ZipFile(zin)
}
