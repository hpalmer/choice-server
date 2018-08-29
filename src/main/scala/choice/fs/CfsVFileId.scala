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
package choice.fs

import choice.model.ResourceId
import choice.fs.vfs.VFileId

case class CfsVFileId(resource : ResourceId) extends VFileId {
    /**
     * Check whether a given file id is the same as this one. The id should
     * always be from the same filesystem as this one.
     *
     * @param id - some file id from the same filesystem
     * @return - true if the file ids match
     */
    override def isFileId_?(id : VFileId) : Boolean = {
        id match {
            case cfsid : CfsVFileId ⇒ cfsid.resource == this.resource
            case _ ⇒ false
        }
    }

    override def toString: String = resource.id.toString
}

object CfsVFileId {
    import language.implicitConversions
    implicit def resourceIdToCfsVFileId(resource : ResourceId) : CfsVFileId = CfsVFileId(resource)

    //    /**
    //     * Make a file id from a raw Resource id value.
    //     *
    //     * @param resource the id of a Cfs Resource
    //     * @return the CfsVFileId
    //     */
    //    def apply(resource : Long) : CfsVFileId = {
    //        val resid = ResourceId(resource)
    //        new CfsVFileId(resid)
    //    }
}
