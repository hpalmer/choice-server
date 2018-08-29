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
package choice.fs.vfs

import java.io.{InputStream, OutputStream}

import choice.access.{HasPrincipal, Principal}
import net.liftweb.common.{Box, EmptyBox, Full}

import scala.annotation.tailrec

/**
 * This serves as the base for an open handle to a file. Typically it will
 * internally reference a specific Vnode, the Vnode on which open() was called to
 * produce the VFile. The VFile subclass may also contain mutable state, which
 * changes in response to operations on the VFile.
 */
trait VFile extends HasPrincipal {

    /**
     * Return the name of this file within its container. Some filesystems allow
     * a file to be linked under multiple containers with different names. This
     * should be the name of the file that was used in the creation of this
     * particular VFile instance whenever possible.
     *
     * @return the file name, normally unique within the container
     */
    def getName : String

    /**
     * Get the filename path associated with this file. Some filesystems may support
     * multiple paths to a file. This should be the path to the file that was used
     * in the creation of this particular VFile instance whenever possible.
     *
     * @return the filename path, normally unique within the filesystem
     */
    def getPath : VPath

    /**
     * Get the file id. Ideally each file in a filesystem should have a unique file id,
     * even when the filesystem supports multiple filename paths to a file. However,
     * some filesystems may not provide access to such ids even when they exist. In
     * those cases, the returned id should at least be the same when the same path
     * is used to access a file.
     *
     * @return the file id
     */
    def getFileId : VFileId = getVnode.getFileId

    /**
     * Get the Principal responsible for operations performed with this VFile.
     * This is normally the Principal passed to the Vnode open() call, but need
     * not be.
     *
     * @return the principal responsible for operations on this VFile
     */
    def getPrincipal : Principal

    /**
     * Retrieve information about the file referenced by this handle. Some
     * filesystems may support options affecting the information returned.
     *
     * @param options optional options for operation
     * @return a VInfo instance
     */
    def info(options : Option[Map[String, Any]]) : Box[VInfo]

    /**
     * Execute some operation supported by this file type. This provides extensibility
     * for different file types. An operation may act on the file, the VFile, or both.
     * An operation may change the how a subsequent getInputStream() or getOutputStream()
     * works.
     *
     * @param op the operation name, which should be recognized by the file type
     * @param options parameters and options for the operation. These depend on
     *                the operation.
     * @return a boxed map of results. If the operation fails, Empty or Failure.
     */
    def execute(op : String, options : Map[String, Any]) : Box[Map[String, Any]] = {
        getVnode execute(this, op, options)
    }

    /**
     * Close the handle. This will also close any open input or output streams,
     * and release any other resources associated with the VFile.
     */
    def close() : Unit

    /**
     * Change the owner of this file. Only the current owner or a system administrator
     * can perform this operation. Once the owner is changed, the file is reopened
     * using the principal who opened it. This will result in a file handle that may
     * have different access rights than before, since the file is under new ownership.
     * That file handle is returned, and the original file handle is closed, so the
     * caller should discard it and use the returned handle.
     *
     * @param principal the principal who will be the new owner of the file
     * @return a boxed file handle for this file, reflecting the new owner
     */
    def chown(principal : Principal) : Box[VFile]

    /**
     * Attempt to mount the specified Vfs filesystem at the file referenced by this handle.
     * Not all filesystems will support this operation, and those that do are likely to
     * restrict mount points to container files. Some options for the mount may be supported,
     * such as mounting read-only, or specifying a password for an encrypted filesystem.
     *
     * If successful, this operation closes the file handle on which it is invoked, and returns
     * a new file handle for the root of the mounted filesystem.
     *
     * @param vfs the filesystem to be mounted
     * @param options optional options that may affect the mount
     * @return a file handle for the root of the mounted filesystem if successful,
     *         otherwise a Failure
     */
    def mount(vfs : Vfs, options : Option[Map[String, Any]] = None) : Box[VFile]

    /**
     * Get the Vnode associated with this file.
     *
     * @return the Vnode
     */
    def getVnode : Vnode
}

/**
 * Mixin trait for VFiles which can return an input stream.
 */
trait CanStreamInput {
    import net.liftweb.util.Helpers.tryo

    /**
     * Get an input stream to read the data associated with this file. Depending
     * on the file type and previously executed operations, the data presented
     * on the input stream may or may not be raw file data. Some file types
     * may not even support an input stream.
     *
     * Any returned input stream will be closed when the VFile is closed, if
     * necessary.
     *
     * @return a boxed input stream if successful. A file that does not support
     *         input streams may return a zero-length stream, Empty, or Failure.
     */
    def getInputStream : Box[InputStream]

    /**
      * Copy the contents of this file to a specified OutputStream. Then close this
      * file.
      *
      * @param out the OutputStream
      * @return boxed count of bytes copied
      */
    def copyToStream(out : OutputStream) : Box[Long] = {
        @tailrec
        def helper(in : InputStream, out : OutputStream, buf : Array[Byte], length : Long) : Box[Long] = {
            tryo(in read buf) match {
                case Full(nread) if nread > 0 ⇒
                    tryo(out write (buf, 0, nread)) match {
                        case Full(_) ⇒ helper(in, out, buf, length + nread)
                        case e : EmptyBox ⇒
                            in close ()
                            e
                    }
                case _ ⇒
                    in close ()
                    Full(length)
            }
        }
        getInputStream match {
            case Full(in : InputStream) ⇒
                val buf = new Array[Byte](8192)
                helper(in, out, buf, 0L)
            case e : EmptyBox ⇒ e
        }
    }
}

/**
 * Mixin trait for VFiles which can return an output stream.
 */
trait CanStreamOutput {
    /**
     * Get an output stream to write data to the associated file. By default,
     * any existing data is overwritten, unless 'append' is specified. The
     * data written on the output stream will not necessarily be written as
     * raw data to the file, depending on file type and previous operations.
     *
     * @param append true if data written to the stream should be appended to
     *               any existing data
     * @param exclusive true if exclusive access to write the file is desired
     * @return a boxed output stream if successful.
     *         A file that does not support output streams normally should return
     *         a Failure
     */
    def getOutputStream(append : Boolean = false, exclusive : Boolean = false) : Box[OutputStream]
}

/**
 * Mixin trait for VFiles which can return file attributes as a map.
 */
trait CanHazMap {
    def asMap : Map[String, Any]
}
