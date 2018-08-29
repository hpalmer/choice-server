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

import net.liftweb.common.{Box, EmptyBox, Full}

trait VDirNode extends Vnode {

    /**
     * Link a given Vnode as a member of this container with a specified name.
     * The name may be modified by some containers, for example, to enforce special
     * syntax constraints. The returned name is the actual name of the member.
     *
     * @param name the filename given to the Vnode in this container
     * @param member the Vnode to be linked as a member
     * @return the boxed name of member if successful, or Failure on error.
     */
    def link(name : String, member : Vnode) : Box[String]

    /**
     * Unlink a member of this container. The member may continue to exist if there are
     * other links to it. And even if there are no other links, the member will not be
     * deleted until the last open file handle for it is closed.
     *
     * @param name the member name
     * @param member the member Vnode
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container.
     */
    def unlink(name : String, member : Vnode) : Box[(Boolean, List[String])]

    /**
     * Check whether this container contains a member with a specified name.
     *
     * @param member the name of the potential member
     * @return true if the principal can ascertain the existence of the member,
     *         false otherwise
     */
    def exists_?(member : String) : Boolean

    /**
     * Get the names of all the members in this container. Optionally, only
     * members of a given MIME type can be returned.
     *
     * @param mimeType an optional MIME type to select only members of this type
     * @return the boxed member names, with an empty sequence indicating there are none.
     *         Failure on error.
     */
    def getMembers(mimeType : Option[String]) : Box[Seq[String]]

    /**
     * Check whether this container is empty, i.e. has no members.
     *
     * @return true if the container is empty, false if not. Also false if
     *         the principal has insufficient access to tell.
     */
    def isEmpty_? : Boolean = getMembers (None) match {
        case Full(seq) ⇒ seq.isEmpty
        case e : EmptyBox ⇒ false
    }

    /**
     * Lookup the member with a given name in this container. This should not be
     * called unless isContainer_?() returns true. If the named member does not
     * exist, Empty is returned.
     *
     * The reference count of the returned Vnode will have been incremented,
     * but it will not be 'open' for read or write operations. At some point
     * the caller should call its release() method.
     *
     * The 'keepParent' argument applies only if the lookup succeeds. If it
     * is false, release() will be called on this VDirNode. If it is true,
     * the caller is responsible for releasing it at some point.
     *
     * @param name the member name
     * @param keepParent true if the parent Vnode should not be released
     * @return a boxed Vnode for the member if successful, Empty if the member
     *         does not exist, or Failure for an error, including access denied
     */
    def lookup(name : String, keepParent : Boolean = false) : Box[Vnode]
}
