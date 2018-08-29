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
package choice.fs.vfs

import choice.fs.{CfsOpenOptions, CfsCreateOptions}
import net.liftweb.common.Box
import choice.access.Principal

/**
 * This trait defines additional operations for VFiles which are containers
 * of other files. This includes both what are usually known as folders or
 * directories, and also any specialized kinds of containers implemented by
 * a filesystem.
 */
trait VDirFile extends VFile {

    /**
     * Create a new member of this container, with a specified name and MIME type.
     * The options may be specific to the type of file being created. The owner of
     * the new file will be the principal associated with this container handle.
     *
     * @param member the name for the new member, which must not already exist,
     *             unless the filesystem implements versioning
     * @param mimeType the MIME type associated with the new file
     * @param options options that may affect the create
     * @return a boxed VFile if the file is created successfully, or a
     *         Failure otherwise. Empty should not be returned.
     */
    def create(member : String, mimeType : String, options : CfsCreateOptions) : Box[VFile]

    /**
     * Link a given file as a member of this container, under a specified name.
     * The specified member file handle is not closed, regardless of the
     * success or failure of the operation.
     *
     * @param name the name to be given the member in this container
     * @param member the file to be linked as a member
     * @return a boxed VFile of the linked member file, with its path referencing
     *         the newly created link. Failure on error.
     */
    def link(name : String, member : VFile) : Box[VFile]

    /**
     * Unlink a member from this container. If this is the last link to the member,
     * it is deleted from the filesystem, and a recursive unlink is performed on its
     * metadata (if metadata is supported). If the member is itself a container, it
     * must be empty (except for metadata), or else the recursive option must be
     * true. A recursive unlink may be partially completed before returning an error.
     *
     * @param member the name of the member to be unlinked from this container
     * @param recursive if the member is a container, recursively unlink its members
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container, or the principal lacked
     *         the proper access rights, or on any other error.
     */
    def unlink(member : String, recursive : Boolean) : Box[Boolean]

    /**
     * Check whether this container contains a member with a specified name.
     *
     * @param member the name of the potential member
     * @return a boxed boolean indicating whether the member exists, if the principal
     *         has the rights to list members. Otherwise Failure.
     */
    def exists_?(member : String) : Box[Boolean]

    /**
     * Get the names of all the members in this container. Optionally, only
     * members of a given MIME type can be returned.
     *
     * @param mimeType an optional MIME type to select only members of this type
     * @return the boxed member names, with an empty sequence indicating there are none.
     *         Failure on error.
     */
    def getMembers(mimeType : Option[String] = None) : Box[Seq[String]]

    /**
     * Check whether this container is empty.
     *
     * @return a boxed boolean indicating whether the container is empty, or
     *         Failure if this cannot be determined by the principal.
     */
    def isEmpty_? : Box[Boolean]

    /**
     * Retrieve information about a member of this container with a specified
     * name. Some filesystems may support options affecting the information
     * returned.
     *
     * @param member the name of member
     * @param options optional options for operation
     * @return a VInfo instance
     */
    def info(member : String, options : Option[Map[String, Any]]) : Box[VInfo]

    /**
     * Open a specified member of this container. This permits a different principal
     * to be used than was used to open this container. The supported options will
     * generally be the same as if the member were opened by its full filename path.
     *
     * @param member the member name within the container
     * @param principal the principal responsible for this operation
     * @param options options which may affect how the file is opened
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    def open(member : String, principal : Principal, options : CfsOpenOptions) : Box[VFile]

    /**
     * Alternate form of open() which uses the principal that was used to open the
     * container.
     *
     * @param member the member name within the container
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    def getMember(member : String) : Box[VFile]

    def getVnode : VDirNode
}
