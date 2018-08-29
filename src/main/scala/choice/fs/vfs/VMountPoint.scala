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

import net.liftweb.common.Box

trait VMountPoint extends VDirNode {

    /**
     * Get the root Vnode of the filesystem mounted on this mount point.
     *
     * @return a boxed Vnode if successful, Empty if no filesystem is mounted,
     *         Failure on error
     */
    def getMount : Box[Vnode]

    /**
     * Check whether a filesystem is mounted on this mount point.
     *
     * @return true if another filesystem is mounted at this node, otherwise false
     */
    def hasMount_? : Boolean

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
     * @return a Vnode for the root of the mounted filesystem if successful,
     *         otherwise a Failure
     */
    def mount(vfs : Vfs, options : Option[Map[String, Any]] = None) : Box[Vnode]

    /**
     * Unmount the filesystem mounted at this node.
     *
     * @return true if a filesystem was unmounted, false if no filesystem was mounted,
     *         Failure on error (typically insufficient permission)
     */
    def unmount : Box[Boolean]
}
