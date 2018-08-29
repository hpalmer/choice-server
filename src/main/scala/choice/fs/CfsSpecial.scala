/**
  * Copyright © 2013-2016 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.fs

import choice.model.Resource
import choice.access.Principal

/**
 * This defines a Vnode for a special file, that is, a file which has an
 * associated MimeTypeHandler for its MIME type.
 *
 * @param resource the DB Resource entry for this file
 */
abstract class CfsSpecialNode(resource : Resource) extends CfsVnode(resource) {
    override def isSpecial_? = true
}

class CfsSpecial(path : CfsAbsolutePath, principal : Principal, vnode : CfsSpecialNode)
    extends CfsFile(path, principal, vnode) {

    override def getVnode : CfsSpecialNode = vnode

    override def isSpecial_? : Boolean = true
}
