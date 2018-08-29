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
  *
  * @author Howard Palmer
  */
package choice.model

trait HasSafeKey[T] {
    def getSafeKey : T
}

/**
 * Every persistent object has a unique resource id. This value class
 * encapsulates a Long that is the key of a Resource entry in the
 * database. The FileManager should be used to access the Resource
 * for a given resource id.
 */
case class ResourceId(id : Long) extends AnyVal {
    override def toString : String = id.toString
}

object ResourceId {
    import language.implicitConversions

    implicit def resourceIdToLong(resid : ResourceId) : Long = resid.id
    implicit def longToResourceId(id : Long) : ResourceId = ResourceId(id)
}

case class DataNodeId(id : Long) extends AnyVal

object DataNodeId {
    import language.implicitConversions
    
    implicit def idToLong(dnid : DataNodeId) : Long = dnid.id
    implicit def longToId(id : Long) : DataNodeId = DataNodeId(id)
}

case class FsNameId(id : Long) extends AnyVal

object FsNameId {
    import language.implicitConversions
    
    implicit def idToLong(fsnid : FsNameId) : Long = fsnid.id
    implicit def longToId(id : Long) : FsNameId = FsNameId(id)
}

case class MimeTypeId(id : Long) extends AnyVal

object MimeTypeId {
    import language.implicitConversions
    
    implicit def idToLong(mimeId : MimeTypeId) : Long = mimeId.id
    implicit def longToId(id : Long) : MimeTypeId = MimeTypeId(id)
}

case class RoleAssnId(id : Long) extends AnyVal

object RoleAssnId {
    import language.implicitConversions

    implicit def idToLong(assnId : RoleAssnId) : Long = assnId.id
    implicit def longToId(id : Long) : RoleAssnId = RoleAssnId(id)
}

case class TimeValue(msec : Long) extends AnyVal

object TimeValue {
    import language.implicitConversions
    
    implicit def tvToLong(tv : TimeValue) : Long = tv.msec
    implicit def longToTv(msec : Long) : TimeValue = TimeValue(msec)
}
