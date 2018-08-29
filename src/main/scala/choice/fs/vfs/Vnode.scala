/**
  * Copyright © 2010-2016 The Board of Trustees of The Leland Stanford Junior University.
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

import choice.fs.CfsOpenOptions
import net.liftweb.common.Box
import choice.access.Principal

/**
 * This is the in-memory object representing a file in some filesystem. It
 * provides the basic interface for accessing a file. Many of the operations
 * supported by the Vnode include a Principal parameter, which is used to
 * authorize access.
 */
trait Vnode {

    /**
     * Execute some operation supported by this file type. This provides extensibility
     * for different file types. An operation may act on the file, the VFile, or both.
     * An operation may change the how a subsequent getInputStream() or getOutputStream()
     * works.
     *
     * @param vhandle a file handle previously returned from open()
     * @param op the operation name, which should be recognized by the file type
     * @param options parameters and options for the operation. These depend on
     *                the operation.
     * @return a boxed map of results. If the operation fails, Empty or Failure.
     */
    def execute(vhandle : VFile, op : String, options : Map[String, Any]) : Box[Map[String, Any]]

    /**
     * Get the VFileId associated with this file. Each file within a particular
     * filesystem should have a unique VFileId.
     *
     * @return a unique id for this file within its filesystem
     */
    def getFileId : VFileId

    /**
     * Retrieve information about the file referenced by this Vnode. Some
     * filesystems may support options affecting the information returned.
     *
     * @param options optional options for operation
     * @return a VInfo instance
     */
    def info(options : Option[Map[String, Any]] = None) : VInfo

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     *
     * @return true if this Vnode represents a container
     */
    def isContainer_? : Boolean

    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * some subclass of VFile that references this Vnode. Using the VFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * @param path the path that led to this Vnode
     * @param principal the principal to be associated with the VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    def open(path : VPath, principal : Principal, options : CfsOpenOptions) : Box[VFile]

    /**
     * Change the owner of the file associated with this Vnode. This operation
     * should be restricted to the current owner of the file, or a system
     * administrator, but that is not done at this level.
     *
     * @param principal the principal who will be the new owner of the file
     * @return the boxed Vnode, with the updated owner
     */
    def chown(principal : Principal) : Box[Vnode]

    /**
     * Acquire a reference to this Vnode. This is normally done automatically
     * on a Vnode returned from a lookup() operation. And the open() operation
     * assumes that the caller has acquired a reference to the Vnode. If open()
     * succeeds then releasing the reference becomes the responsibility of the
     * returned VFile. If open() fails, the reference is automatically released.
     *
     * @return the current number of dynamic references to the Vnode
     */
    def acquire : Long

    /**
     * Release a previously acquired reference to this Vnode. The remaining
     * number of references is returned. The Vnode becomes eligible for
     * deallocation when the count goes to zero, but is not necessarily
     * deallocated immediately. An exception may be thrown if release()
     * is called when the count is zero.
     *
     * @return the number of references to this Vnode remaining
     */
    def release : Long

    def isReferenced_? : Boolean
}
