/**
  * Copyright © 2016-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Created by Hep on 11/20/2016.
  */
package choice.fs

import java.io._
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.channels.SeekableByteChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file._
import java.util.concurrent.atomic.AtomicBoolean

import choice.access.{CfsPrincipal, Principal}
import choice.actor.DbManager
import choice.attributes.AttrVal
import choice.fs.vfs.AttributeId
import choice.model.ResourceId
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo

import scala.annotation.tailrec

case class MIME_TYPE(mimetype : String) extends CopyOption with OpenOption

case object WITH_ATTRIBUTES extends CopyOption

case object WITH_POLICIES extends CopyOption

trait CloseActions[T <: Closeable] extends Closeable { self : T ⇒

    private var _closeActions = List.empty[T ⇒ Unit]

    def onClose(f : T ⇒ Unit) : Unit = {
        _closeActions = f :: _closeActions
    }

    abstract override def close() : Unit = {
        super.close()
        handleClose()
    }

    protected def handleClose() : Unit = {
        val closeActions = _closeActions
        closeActions foreach (_(self))
    }
}

class CfsCloseable extends Closeable {
    def close() : Unit = ()
}

class CfsInputStream(cfsfile : Option[CfsFile], wrappedStream : InputStream) extends InputStream
        with CloseActions[CfsInputStream] {

    protected val _closed = new AtomicBoolean(false)

    override def available() : Int = wrappedStream.available()

    override def mark(readlimit : Int) : Unit = wrappedStream.mark(readlimit)

    override def skip(n : Long) : Long = wrappedStream.skip(n)

    override def markSupported() : Boolean = wrappedStream.markSupported()

    override def close() : Unit = {
        if (!_closed.getAndSet(true)) {
            wrappedStream close ()
            cfsfile foreach { f ⇒
                if (f.isClosed) {
                    Cfs.Log.error(s"${f.getPath.toString} was closed before the InputStream using it")
                }
                else {
                    f close ()
                }
            }
        }
        else cfsfile.foreach { f ⇒
            if (!f.isClosed) {
                Cfs.Log.error(s"InputStream for ${f.getPath.toString} was closed but not the file")
                f close ()
            }
        }
    }

    override def read() : Int = wrappedStream.read()

    override def read(b : Array[Byte]) : Int = wrappedStream.read(b)

    override def read(b : Array[Byte], off : Int, len : Int) : Int = wrappedStream.read(b, off, len)

    override def reset() : Unit = wrappedStream.reset()

    def getMimeType : String = cfsfile flatMap (_.getMimeType) getOrElse ""
}

class CfsOutputStream(cfsfile: CfsFile, wrappedStream : OutputStream) extends OutputStream
        with CloseActions[CfsOutputStream] {

    protected val _closed = new AtomicBoolean(false)

    override def flush() : Unit = wrappedStream.flush()

    override def write(b : Int) : Unit = wrappedStream.write(b)

    override def write(b : Array[Byte]) : Unit = wrappedStream.write(b)

    override def write(b : Array[Byte], off : Int, len : Int) : Unit = wrappedStream.write(b, off, len)

    override def close() : Unit = {
        if (!_closed.getAndSet(true)) {
            wrappedStream close ()
            if (cfsfile.isClosed) {
                Cfs.Log.error(s"${cfsfile.getPath.toString} was closed before the InputStream using it")
            }
            else {
                cfsfile close ()
            }
        }
        else if (!cfsfile.isClosed) {
            Cfs.Log.error(s"InputStream for ${cfsfile.getPath.toString} was closed but not the file")
            cfsfile close ()
        }
    }

    def getMimeType : String = cfsfile.getMimeType openOr ""
}

/**
  * A SeekableByteChannel for a Cfs file. The actual channel is normally on the
  * associated ChoiceData file. However, for read-only access to an empty Cfs file,
  * there is no ChoiceData File, so wrapped will be None. Whenever there is write
  * access, there will be a ChoiceData file, even if it is empty.
  *
  * @param cfsfile the associated Cfs file
  * @param wrapped usually a SeekableByteChannel on the associated ChoiceData file
  */
class CfsByteChannel(cfsfile : CfsPlain, wrapped : Option[SeekableByteChannel])
    extends CfsCloseable with SeekableByteChannel with CloseActions[CfsByteChannel] {

    protected val _closed = new AtomicBoolean(false)

    override def position() : Long = wrapped.map(_.position()).getOrElse(0L)

    override def position(newPosition : Long) : SeekableByteChannel = wrapped match {
        case Some(chan) ⇒ chan.position(newPosition)
        case None ⇒
            if (newPosition == 0L) this
            else throw new IOException(s"${cfsfile.getPath.toString}: attempt to position empty file")
    }

    override def size() : Long = wrapped.map(_.size()).getOrElse(0L)

    override def truncate(size : Long) : SeekableByteChannel = wrapped match {
        case Some(chan) ⇒ chan.truncate(size)
        case None ⇒ throw new IOException(s"${cfsfile.getPath.toString}: no write access")
    }

    override def write(src : ByteBuffer) : Int = wrapped match {
        case Some(chan) ⇒ chan.write(src)
        case None ⇒ throw new IOException(s"${cfsfile.getPath.toString}: no write access")
    }

    override def read(dst : ByteBuffer) : Int = wrapped match {
        case Some(chan) ⇒ chan.read(dst)
        case None ⇒ -1
    }

    override def isOpen : Boolean = wrapped.map(_.isOpen).getOrElse(!_closed.get())

    override def close() : Unit = {
        if (!_closed.getAndSet(true)) {
            wrapped foreach (_ close ())
            if (cfsfile.isClosed) {
                Cfs.Log.error(s"${cfsfile.getPath.toString} was closed before the InputStream using it")
            }
            else {
                cfsfile close ()
            }
        }
        else if (!cfsfile.isClosed) {
            Cfs.Log.error(s"InputStream for ${cfsfile.getPath.toString} was closed but not the file")
            cfsfile close ()
        }
    }
}

class CfsBufferedReader(cfsin : CfsInputStream, cs : Charset) extends BufferedReader(new InputStreamReader(cfsin, cs))
    with CloseActions[CfsBufferedReader] {

    def readAll() : String = {
        val sbuf = new StringBuilder(1024)
        val buf = CharBuffer.allocate(1024)
        def helper() : String = {
            buf.clear()
            val n = read(buf)
            if (n > 0) {
                buf.flip()
                sbuf.append(buf.toString)
                helper()
            }
            else {
                close()
                sbuf.toString()
            }
        }
        helper()
    }
}

class CfsBufferedWriter(cfsout : CfsOutputStream, cs : Charset)
    extends BufferedWriter(new OutputStreamWriter(cfsout, cs)) with CloseActions[CfsBufferedWriter] {}

/**
  * This object is patterned after java.nio.file.Files, except that it operates on Cfs files.
  * A better solution would be to implement an NIO FileSystem for Cfs files, but that is
  * quite a lot of work.
  *
  * These functions should be called where the principal for access control purposes
  * can be retrieved by an implicit function.
  */
object CfsFiles {

    /** Tuple for expanding open options **/
    type OptionTuple = (Boolean, Boolean, Boolean, Boolean, Boolean, Option[String])

    /**
      * Expand a sequence of OpenOption into a OptionTuple.
      *
      * @param optionList the OpenOption sequence
      * @param result the initial value of the result
      * @return the result tuple
      */
    @tailrec
    def optionHelper(optionList : Seq[OpenOption], result : OptionTuple) : OptionTuple = {
        optionList.headOption match {
            case None ⇒ result
            case Some(StandardOpenOption.READ) ⇒
                optionHelper(optionList.tail, (true, result._2, result._3, result._4, result._5, result._6))
            case Some(StandardOpenOption.WRITE) ⇒
                optionHelper(optionList.tail, (result._1, true, result._3, result._4, result._5, result._6))
            case Some(StandardOpenOption.APPEND) ⇒
                optionHelper(optionList.tail, (result._1, result._2, true, result._4, result._5, result._6))
            case Some(StandardOpenOption.CREATE) ⇒
                optionHelper(optionList.tail, (result._1, result._2, result._3, true, result._5, result._6))
            case Some(StandardOpenOption.CREATE_NEW) ⇒
                optionHelper(optionList.tail, (result._1, result._2, result._3, result._4, true, result._6))
            case Some(MIME_TYPE(mt)) ⇒
                optionHelper(optionList.tail, (result._1, result._2, result._3, result._4, result._5, Some(mt)))
            case _ ⇒ optionHelper(optionList.tail, result)
        }
    }

    protected def nioOpenOptions(options : Seq[OpenOption]) : Seq[OpenOption] = {
        options filterNot { opt ⇒ opt.isInstanceOf[MIME_TYPE] }
    }

    protected def nioCopyOptions(options : Seq[CopyOption]) : Seq[CopyOption] = {
        options filterNot { opt ⇒
            opt.isInstanceOf[MIME_TYPE] || opt == WITH_ATTRIBUTES || opt == WITH_POLICIES
        }
    }

    /**
      * Copy an input stream to a specified file. If the specified output file already exists,
      * it must be a plain file, and the StandardCopyOption.REPLACE_EXISTING option must be
      * included in the options. File metadata attributes and access control policies are not
      * modified on a pre-existing output file.
      *
      * If the output file does not already exist, it is created with a MIME type of "text/plain"
      * unless the options include a MIME_TYPE option specifying otherwise.
      *
      * The input stream is left open.
      *
      * @param in the input stream
      * @param target the absolute path of the output file
      * @param options copy options
      * @param principal a function returning the principal for this operation
      * @return the number of bytes copied
      */
    def copy(in : InputStream, target : CfsAbsolutePath, options : CopyOption*)
            (implicit principal : () ⇒ Principal) : Long = {
        val rbox = Cfs open (target, principal(), CfsOpenOptions.Default) match {
            case Full(cfsplain : CfsPlain) ⇒
                try {
                    if (!options.contains(java.nio.file.StandardCopyOption.REPLACE_EXISTING)) {
                        throw new FileAlreadyExistsException(target.toString)
                    }
                    cfsplain.getUniqueData flatMap {
                        case (_, niopath) ⇒
                            val result = Files.copy(in, niopath, nioCopyOptions(options) : _*)
                            Full(result)
                    }
                }
                finally {
                    cfsplain close ()
                }
            case Full(other) ⇒
                other close ()
                Failure(s"${target.toString} is not a plain file")
            case Empty ⇒
                val mimetype = options.collectFirst {
                    case MIME_TYPE(mt) ⇒ mt
                } getOrElse "text/plain"
                Cfs create (target, principal(), mimetype, CfsCreateOptions.Default) match {
                    case Full(cfsplain : CfsPlain) ⇒
                        try {
                            cfsplain.getUniqueData flatMap {
                                case (_, niopath) ⇒
                                    val withReplace = StandardCopyOption.REPLACE_EXISTING +: nioCopyOptions(options)
                                    val result = Files.copy(in, niopath, withReplace : _*)
                                    Full(result)
                            }
                        }
                        catch {
                            case ex : Throwable ⇒ Failure(s"${target.toString}:", Full(ex), Empty)
                        }
                        finally {
                            cfsplain close()
                        }
                    case Full(other) ⇒
                        other close ()
                        Failure(s"MIME type $mimetype did not result in a plain file")
                    case e : EmptyBox ⇒ e ?~ "Empty"
                }
            case f : Failure ⇒ f
        }
        rbox match {
            case Full(nbytes) ⇒ nbytes
            case e : EmptyBox ⇒ throw new IOException((e ?~ "Empty").messageChain)
        }
    }

    /**
      * Copy an existing file to an output stream. The existing file must be a plain file.
      *
      * The output stream is left open.
      *
      * @param source the absolute path of the existing file
      * @param out the output stream
      * @param principal a function returning the principal for this operation
      * @return the number of bytes copied
      */
    def copy(source : CfsAbsolutePath, out : OutputStream)(implicit principal : () ⇒ Principal) : Long = {
        val rbox = Cfs.withExistingFile(source, principal(), CfsOpenOptions.Default) {
            case cfsplain : CfsPlain ⇒
                cfsplain.getInputData match {
                    case Full((_, niopath)) ⇒ Full(Files.copy(niopath, out))
                    case Empty ⇒ Full(0L)
                    case f : Failure ⇒ f
                }
        }
        rbox match {
            case Full(nbytes) ⇒ nbytes
            case e : EmptyBox ⇒ throw new IOException((e ?~ "Empty").messageChain)
        }
    }

    def copy(source : CfsAbsolutePath, target : CfsAbsolutePath, options : CopyOption*)
            (implicit principal : () ⇒ Principal) : Long = {
        val rbox = Cfs.withExistingFile(source, principal(), CfsOpenOptions.Default) {
            case cfsplain : CfsPlain ⇒
                val optionsWithMimeType = {
                    if (options.exists(_.isInstanceOf[MIME_TYPE])) options
                    else cfsplain.getMimeType map (mt ⇒ MIME_TYPE(mt) +: options) openOr options
                }
                cfsplain.getInputData match {
                    case Full((_, niopath)) ⇒
                        val in = Files.newInputStream(niopath)
                        tryo(copy(in, target, optionsWithMimeType : _*)(principal))
                    case Empty ⇒
                        tryo(copy(new ByteArrayInputStream(new Array[Byte](0)), target, optionsWithMimeType : _*))
                    case f : Failure ⇒ f
                }
        }
        rbox match {
            case Full(nbytes) ⇒
                if (options contains WITH_ATTRIBUTES) {
                    copyAttributes(source, target)
                }
                if (options contains WITH_POLICIES) {
                    copyPolicies(source, target)
                }
                nbytes
            case e : EmptyBox ⇒ throw new IOException((e ?~ "Empty").messageChain)
        }
    }

    /**
      * Copy file metadata attributes from one file to another. Only attributes which can be
      * read by the principal are copied. The principal becomes the setter for each attribute
      * set on the target file.
      *
      * @param source file with attributes to be copied
      * @param target target file for copied attributes
      * @param principal a function returning the principal for this operation
      * @return the number of attributes copied
      */
    def copyAttributes(source : CfsAbsolutePath, target : CfsAbsolutePath)
                      (implicit principal : () ⇒ Principal) : Long = {
        var copyCount = 0L
        Cfs.withExistingFile(source, principal()) {
            case infile : CfsFile ⇒
                Cfs.withExistingFile(target, principal()) {
                    case outfile : CfsFile ⇒
                        AttrVal getAll infile.getResourceId foreach { attrval ⇒
                            infile getAttribute (AttributeId(attrval.id.get), None) foreach { attinfo ⇒
                                outfile setAttribute (attinfo.id, Some(attinfo.atype), attinfo.value) foreach { _ ⇒
                                    copyCount += 1
                                }
                            }
                        }
                        Full(copyCount)
                }
        }
        copyCount
    }

    def copyPolicies(source : CfsAbsolutePath, target : CfsAbsolutePath)
                    (implicit principal : () ⇒ Principal) : Box[Boolean] = {
        val thePrincipal = principal()
        Cfs.withExistingFile(source, thePrincipal) {
            case infile : CfsFile ⇒
                Cfs.withExistingFile(target, thePrincipal) {
                    case outfile : CfsFile ⇒
                        val outvnode = outfile.getVnode
                        if (thePrincipal.isSystemAdmin_? || outvnode.getOwnerId == thePrincipal.getPrincipalId || {
                            val ownerPrincipal = CfsPrincipal(ResourceId(outvnode.getOwnerId))
                            ownerPrincipal.isGroup_? && thePrincipal.isDescendantOf(outvnode.getOwnerId)
                        }) {
                            infile.getVnode.getPolicyRefs foreach { policy ⇒ outvnode addPolicy policy.getPolicy }
                            Full(true)
                        }
                        else Full(false)
                }
        }
    }

    def newBufferedReader(path : CfsAbsolutePath)(implicit principal : () ⇒ Principal) : CfsBufferedReader = {
        newBufferedReader(path, StandardCharsets.UTF_8)
    }

    def newBufferedReader(path : CfsAbsolutePath, cs : Charset)
                         (implicit principal : () ⇒ Principal) : CfsBufferedReader = {
        new CfsBufferedReader(newInputStream(path)(principal), cs)
    }

    def newBufferedReader(cfsplain : CfsPlain) : CfsBufferedReader = newBufferedReader(cfsplain, StandardCharsets.UTF_8)

    def newBufferedReader(cfsplain : CfsPlain, cs : Charset) : CfsBufferedReader = {
        new CfsBufferedReader(newInputStream(cfsplain), cs)
    }

    def newBufferedWriter(path : CfsAbsolutePath, options : OpenOption*)
                         (implicit principal: () ⇒ Principal) : CfsBufferedWriter = {
        newBufferedWriter(path, StandardCharsets.UTF_8, options : _*)(principal)
    }

    def newBufferedWriter(path : CfsAbsolutePath, cs : Charset, options : OpenOption*)
                         (implicit principal : () ⇒ Principal) : CfsBufferedWriter = {
        new CfsBufferedWriter(newOutputStream(path, options : _*), cs)
    }

    def newBufferedWriter(cfsplain : CfsPlain, options : OpenOption*) : CfsBufferedWriter = {
        newBufferedWriter(cfsplain, StandardCharsets.UTF_8)
    }

    def newBufferedWriter(cfsplain : CfsPlain, cs : Charset, options : OpenOption*) : CfsBufferedWriter = {
        new CfsBufferedWriter(newOutputStream(cfsplain, options : _*), cs)
    }

    def newByteChannel(path : CfsAbsolutePath, options : OpenOption*)
                      (implicit principal : () ⇒ Principal) : CfsByteChannel = {
        val optionTuple = optionHelper(options.toList, (false, false, false, false, false, None))
        val (_, writePresent, appendPresent, createPresent, createNewPresent, mimetypeOpt) = optionTuple
        val needWrite = writePresent || appendPresent
        Cfs open (path, principal(), CfsOpenOptions.Default) match {
            case Full(cfsplain : CfsPlain) ⇒
                if (!needWrite || !createNewPresent) newByteChannel(cfsplain, optionTuple, options : _*)
                else throw new FileAlreadyExistsException(s"${path.toString} already exists")
            case Full(other) ⇒
                other close ()
                throw new IOException(s"${path.toString} is not a plain file")
            case Empty ⇒
                if (needWrite && (createPresent || createNewPresent)) {
                    val mimetype = mimetypeOpt getOrElse "text/plain"
                    DbManager getMimeTypeHandler mimetype match {
                        case Full(mth) ⇒
                            // Only plain files can be created
                            if (mth.isSpecial_?) {
                                throw new IOException(s"operation not supported for MIME type $mimetype")
                            }
                            else {
                                Cfs create(path, principal(), mimetype, CfsCreateOptions.Default) match {
                                    case Full(cfsplain : CfsPlain) ⇒ newByteChannel(cfsplain, optionTuple, options : _*)
                                    case Full(other) ⇒
                                        // Shouldn't happen since we checked the MIME type
                                        other close()
                                        throw new IOException(s"MIME type $mimetype did not result in a plain file for ${path.toString}")
                                    case e : EmptyBox ⇒
                                        throw new IOException(s"failed to create ${path.toString}: ${(e ?~ "Empty").messageChain}")
                                }
                            }
                        case _ : EmptyBox ⇒
                            throw new IOException(s"no MIME type handler for $mimetype")
                    }
                }
                else throw new IOException(s"${path.toString} does not exist and no create option")
            case f : Failure ⇒
                throw new IOException(s"error accessing output file ${path.toString}: ${f.messageChain}")
        }
    }

    def newByteChannel(cfsplain : CfsPlain, options : OpenOption*) : CfsByteChannel = {
        newByteChannel(cfsplain, optionHelper(options, (false, false, false, false, false, None)), options : _*)
    }

    def newByteChannel(cfsplain : CfsPlain, optionTuple : OptionTuple, options : OpenOption*) : CfsByteChannel = {
        val (readPresent, writePresent, appendPresent, _, _, _) = optionTuple
        val needRead = readPresent || !(writePresent || appendPresent)
        val needWrite = writePresent || appendPresent
        try {
            val pathbox = {
                if (needWrite) {
                    cfsplain.getUniqueData flatMap { tuple ⇒
                        if (needRead) cfsplain.getInputData
                        else Full(tuple)
                    }
                }
                else if (needRead) cfsplain.getInputData
                else {
                    cfsplain close()
                    throw new IllegalArgumentException(s"${cfsplain.getPath.toString}: no read or write option")
                }
            }
            pathbox match {
                case Full((_, niopath)) ⇒
                    val chan = Files.newByteChannel(niopath, nioOpenOptions(options) : _*)
                    new CfsByteChannel(cfsplain, Some(chan))
                case Empty ⇒
                    if (!needWrite) new CfsByteChannel(cfsplain, None)
                    else throw new IOException(s"${cfsplain.getPath.toString}: no ChoiceData file for write access")
                case f : Failure ⇒ throw new IOException(s"${cfsplain.getPath.toString}: ${f.messageChain}")
            }
        }
        catch {
            case ex : Throwable ⇒
                cfsplain close ()
                throw ex
        }
    }

    def newInputStream(path : CfsAbsolutePath, options : OpenOption*)
                      (implicit principal : () ⇒ Principal) : CfsInputStream = {
        Cfs open (path, principal(), CfsOpenOptions.Default) match {
            case Full(cfsplain : CfsPlain) ⇒ newInputStream(cfsplain, options : _*)
            case Full(other) ⇒
                other close ()
                throw new IOException(s"${path.toString} is not a plain file")
            case Empty ⇒ throw new IOException(s"${path.toString} does not exist")
            case f : Failure ⇒ throw new IOException(f.messageChain)
        }
    }

    /**
      * This makes an InputStream for a plain file. The InputStream wraps the file,
      * so that when the InputStream is closed, the file is also closed. If the
      * InputStream cannot be created for any reason, the specified file will be
      * closed, and an IOException thrown.
      *
      * @param cfsplain a plain file handle
      * @param options open options, passed to Java untouched
      * @return
      */
    def newInputStream(cfsplain : CfsPlain, options : OpenOption*) : CfsInputStream = {
        val inbox = cfsplain.getInputData match {
            case Full((_, niopath)) ⇒
                tryo(Files.newInputStream(niopath, nioOpenOptions(options) : _*)) map { in ⇒
                    new CfsInputStream(Some(cfsplain), in)
                }
            case Empty ⇒
                Full(new CfsInputStream(Some(cfsplain), new ByteArrayInputStream(new Array[Byte](0))))
            case f : Failure ⇒ f
        }
        inbox match {
            case Full(in) ⇒ in
            case e : EmptyBox ⇒
                cfsplain close ()
                throw new IOException((e ?~ "Empty").messageChain)
        }
    }

    def newOutputStream(path : CfsAbsolutePath, options : OpenOption*)
                       (implicit principal : () ⇒ Principal) : CfsOutputStream = {
        val safeOptions =
            if (options.isEmpty) {
                Seq(StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
                )
            }
            else nioOpenOptions(options)
        val mimetypeOpt = options collectFirst {
            case MIME_TYPE(mt) ⇒ mt
        }
        Cfs open (path, principal(), CfsOpenOptions.Default) match {
            case Full(cfsplain : CfsPlain) ⇒
                if (safeOptions.contains(StandardOpenOption.CREATE_NEW)) {
                    throw new IOException(s"CREATE_NEW specified and ${path.toString} already exists")
                }
                else {
                    // If a MIME type was specified as an option, it must match the MIME type
                    // of the existing file.
                    val mtok = mimetypeOpt.fold(true) { mt ⇒ cfsplain.getMimeType.map(_ == mt).openOr(false) }
                    if (mtok) {
                        newOutputStream(cfsplain, safeOptions : _*)
                    }
                    else {
                        cfsplain close()
                        throw new IOException(s"MIME type mismatch on output file ${path.toString}")
                    }
                }
            case Full(other) ⇒
                other close ()
                throw new IOException(s"cannot make OutputStream for ${path.toString}")
            case Empty ⇒
                if (!safeOptions.exists(o ⇒ o == StandardOpenOption.CREATE || o == StandardOpenOption.CREATE_NEW)) {
                    throw new IOException(s"${path.toString} does not exist and no creation option")
                }
                else {
                    val mimetype = mimetypeOpt getOrElse "text/plain"
                    DbManager getMimeTypeHandler mimetype match {
                        case Full(mth) ⇒
                            // Only plain files can be created with a string
                            if (mth.isSpecial_?) {
                                throw new IOException(s"operation not supported for MIME type $mimetype")
                            }
                            else {
                                Cfs create(path, principal(), mimetype, CfsCreateOptions.Default) match {
                                    case Full(cfsplain : CfsPlain) ⇒ newOutputStream(cfsplain, safeOptions : _*)
                                    case Full(other) ⇒
                                        // Shouldn't happen since we checked the MIME type
                                        other close()
                                        throw new IOException(s"MIME type $mimetype did not result in a plain file for ${path.toString}")
                                    case e : EmptyBox ⇒
                                        throw new IOException(s"failed to create ${path.toString}: ${(e ?~ "Empty").messageChain}")
                                }
                            }
                        case _ : EmptyBox ⇒
                            throw new IOException(s"no MIME type handler for $mimetype")
                    }
                }
            case f : Failure ⇒
                throw new IOException(s"error accessing output file ${path.toString}: ${f.messageChain}")
        }
    }

    def newOutputStream(cfsplain : CfsPlain, options : OpenOption*) : CfsOutputStream = {
        // The file has already been created, so remove CREATE_NEW
        val safeOptions =
            if (options.isEmpty) {
                Seq(StandardOpenOption.CREATE,
                    StandardOpenOption.TRUNCATE_EXISTING,
                    StandardOpenOption.WRITE
                )
            }
            else nioOpenOptions(options).filterNot(_ == StandardOpenOption.CREATE_NEW)
        val outbox = cfsplain.getUniqueData flatMap {
            case (_, niopath) ⇒
                tryo(Files.newOutputStream(niopath, safeOptions : _*)) map { out ⇒
                    new CfsOutputStream(cfsplain, out)
                }
        }
        outbox match {
            case Full(out) ⇒ out
            case e : EmptyBox ⇒
                cfsplain close ()
                throw new IOException((e ?~ "Empty").messageChain)
        }
    }
}
