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
package choice.script.lua

import java.nio.ByteBuffer
import java.nio.file.StandardOpenOption

import choice.access.Principal
import choice.fs._
import net.liftweb.common._
import net.liftweb.util.Helpers.tryo
import org.luaj.vm2.lib.IoLib
import org.luaj.vm2.{LuaError, LuaString}

/**
 * Lua IoLib extension for Cfs.
 * Created by Hep on 5/18/2014.
 */
class FsIoLib(chenv : ChoiceEnviron) extends IoLib {

    private var _openFiles : List[FileImpl] = Nil

    def getPrincipal : Principal = chenv.principal

    def closeAll() : Unit = {
        val stillOpen = _openFiles
        for (f ← stillOpen) f close ()
        _openFiles = Nil
    }

    /**
      * This is modeled after the FileImpl class in JseIoLib.java, except that it operates on
      * wrapped Cfs files. This FileImpl should always be constructed with one of the constructors
      * in the companion object. That is, exactly one of its arguments should not be None.
      *
      * @param inputStream a Cfs input stream
      * @param outputStream a Cfs output stream
      * @param randomAccessFile a Cfs byte channel
      */
    private class FileImpl(inputStream : Option[CfsInputStream], outputStream : Option[CfsOutputStream],
                           randomAccessFile : Option[CfsByteChannel]) extends File {

        _openFiles = this :: _openFiles

        private var closed : Boolean = false
        private var nobuffer : Boolean = false

        override def isstdfile() : Boolean = false

        override def close() : Unit = {
            randomAccessFile foreach (_.close())
            outputStream foreach (_.close())
            inputStream foreach (_.close())
            closed = true
            // Lua has an eq. Want the Scala eq here.
            _openFiles = _openFiles filterNot (f ⇒ f.asInstanceOf[AnyRef].eq(this))
        }

        override def flush() : Unit = {
            outputStream foreach (_.flush())
        }

        override def write(string : LuaString) : Unit = {
            outputStream match {
                case Some(out) ⇒
                    out.write(string.m_bytes, string.m_offset, string.m_length)
                    if (nobuffer) out.flush()
                case None ⇒
                    randomAccessFile match {
                        case Some(chan) ⇒
                            val buf = ByteBuffer.wrap(string.m_bytes, string.m_offset, string.m_length)
                            chan.write(buf)
                        case None ⇒ notImplemented()
                    }
            }
        }

        override def isclosed() : Boolean = closed

        override def seek(option : String, bytecount : Int) : Int = {
            randomAccessFile match {
                case Some(chan) ⇒
                    val pos = option match {
                        case "set" ⇒ bytecount.toLong
                        case "end" ⇒ chan.size() + bytecount
                        case _ ⇒ chan.position() + bytecount
                    }
                    chan.position(pos).position().toInt
                case None ⇒ notImplemented()
            }
        }

        override def setvbuf(mode : String, size : Int) : Unit = {
            nobuffer = mode == "no"
        }

        override def remaining() : Int = {
            randomAccessFile map (chan ⇒ (chan.size() - chan.position()).toInt) getOrElse -1
        }

        override def peek() : Int = {
            inputStream match {
                case Some(in) ⇒
                    in.mark(1)
                    val c = in.read()
                    in.reset()
                    c
                case None ⇒
                    randomAccessFile match {
                        case Some(chan) ⇒
                            val pos = chan.position()
                            val buf = ByteBuffer.allocate(1)
                            val n = chan.read(buf)
                            chan.position(pos)
                            if (n == 1) buf.array()(0).toInt & 0xff
                            else -1
                        case None ⇒ notImplemented()
                    }
            }
        }

        override def read() : Int = {
            inputStream match {
                case Some(in) ⇒ in.read()
                case None ⇒
                    randomAccessFile match {
                        case Some(chan) ⇒
                            val buf = ByteBuffer.allocate(1)
                            val n = chan.read(buf)
                            if (n == 1) buf.array()(0).toInt & 0xff
                            else -1
                        case None ⇒ notImplemented()
                    }
            }
        }

        override def read(bytes : Array[Byte], offset : Int, length : Int) : Int = {
            randomAccessFile match {
                case Some(chan) ⇒
                    val buf = ByteBuffer.wrap(bytes, offset, length)
                    chan.read(buf)
                case None ⇒
                    inputStream match {
                        case Some(in) ⇒ in.read(bytes, offset, length)
                        case None ⇒ notImplemented()
                    }
            }
        }
    }

    private object FileImpl {

        def apply(inputStream : CfsInputStream) : FileImpl = new FileImpl(Some(inputStream), None, None)

        def apply(outputStream : CfsOutputStream) : FileImpl = new FileImpl(None, Some(outputStream), None)

        def apply(randomAccessFile : CfsByteChannel) : FileImpl = new FileImpl(None, None, Some(randomAccessFile))
    }

    private def notImplemented() : Nothing = {
        throw new LuaError("not implemented")
    }

    override def wrapStdin(): File = notImplemented()

    override def openFile(filename: String, readMode: Boolean, appendMode: Boolean,
                          updateMode: Boolean, binaryMode: Boolean): File = {
        implicit val principal : () ⇒ Principal = getPrincipal _
        val cfsFileLib = chenv.cfsFileLib
        val fimplBox = cfsFileLib.getAbsolutePath(filename) flatMap { path ⇒
            val options = List(
                if (readMode) List(StandardOpenOption.READ)
                else List(StandardOpenOption.READ, StandardOpenOption.WRITE, StandardOpenOption.CREATE),
                if (appendMode) List(StandardOpenOption.APPEND)
                else if (!readMode) List(StandardOpenOption.TRUNCATE_EXISTING) else Nil,
                if (binaryMode) List(MIME_TYPE("application/octet-stream")) else Nil
            ).flatten
            tryo(CfsFiles.newByteChannel(path, options : _*)) map (ch ⇒ FileImpl(ch))
        }
        fimplBox match {
            case Full(file) ⇒ file
            case Empty ⇒ throw new LuaError(s"$filename: Empty result in openFile()")
            case f : Failure ⇒ throw new LuaError(s"$filename: ${f.messageChain}")
        }
    }

    override def tmpFile() : File = notImplemented()

    override def openProgram(prog: String, mode: String) : File = notImplemented()

    override def wrapStdout() : File = notImplemented()

    override def wrapStderr() : File = notImplemented()
}
