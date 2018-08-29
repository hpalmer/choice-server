/**
  * Copyright © 2012-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  *
  */
package choice.model

import choice.actor.DbTableCache
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.ControlHelpers._

import scala.collection.mutable

/**
 * The MimeType table is used to associate a unique integer id with a
 * MIME type string.
 */
class MimeType extends LongKeyedMapper[MimeType] with IdPK with HasSafeKey[MimeTypeId] {
    final def getSingleton : MimeType.type = MimeType
    object mtstring extends MappedString(this, 255) {
        override def dbNotNull_? = true
    }
    
    /** Get the key for this entry wrapped as a MimeTypeId */
    def getSafeKey : MimeTypeId = MimeTypeId(this.id.get)
}

object MimeType extends MimeType with LongKeyedMetaMapper[MimeType] {
    private val Log = Logger("choice.model.MimeType")

    override def dbIndexes : List[BaseIndex[MimeType]] = UniqueIndex(mtstring) :: super.dbIndexes

    private object cache extends DbTableCache[String, MimeType](MimeType) {
        // Map of MIME type ids to MimeTypes
        val idmap : mutable.Map[MimeTypeId, MimeType] = mutable.Map[MimeTypeId, MimeType]()

        /**
         * Get the MimeType entry for a given MimeTypeId. The entry should always
         * exist in the database, if not in the cache.
         *
         * @param id the MimeTypeId of the desired entry
         * @return the boxed entry
         */
        def get(id : MimeTypeId) : Box[MimeType] = {
            idmap get id match {
                case None ⇒
                    MimeType findByKey id.id match {
                        case full @ Full(m) ⇒
                            cache(m)
                            full
                        case e : EmptyBox ⇒
                            Log.error(s"MimeTypeId ${id.id} not found: ${(e ?~ "Empty").messageChain}")
                            e
                    }
                case Some(m) ⇒ Full(m)
            }
        }

        /**
         * Fetch the element with the given key from the database. The key can
         * contain anything which is sufficient to construct a query that will
         * return a unique table element. However, this function probably does
         * not guarantee the uniqueness.
         *
         * @param key the key to use to find the element in the database
         * @return the boxed entry corresponding to the key
         */
        override def fetch(key: String): Box[MimeType] = {
            MimeType.find(By(MimeType.mtstring, key)) match {
                case full @ Full(m) ⇒
                    idmap put (m.getSafeKey, m)
                    full
                case e : EmptyBox ⇒ e
            }
        }

        /**
         * Cache an element if it's not already cached.
         *
         * @param elem the element to be cached
         */
        override def cache(elem: MimeType): Unit = {
            cache(elem.mtstring.get, elem)
            idmap put (elem.getSafeKey, elem)
        }

        /**
         * Evict the element that has specified id, if it is in the cache. This does not
         * check whether canEvict() is true, and does not call evicted().
         *
         * @param key unique key of element
         *
         * @return true if the element was in the cache
         */
        override def evict(key: String): Boolean = {
            rawget(key) foreach { idmap remove _.getSafeKey }
            super.evict(key)
        }

        /**
         * This is called when an entry is evicted due to a limit on the number of entries.
         * It is not called when an entry is explicitly evicted by calling evict().
         *
         * @param key the entry key
         * @param value the entry value
         */
        override def evicted(key: String, value: MimeType): Unit = {
            super.evicted(key, value)
            idmap remove value.getSafeKey
        }
    }

    def get(id : MimeTypeId) : Box[MimeType] = cache get id

    def getId(mimeType : String) : Box[MimeTypeId] = {
        cache get mimeType map (_.getSafeKey)
    }

    /**
     * Find or create an entry for a given MIME type string, and return its id value.
     *
     * @param ts	the MIME type string
     * @return		the unique id associated with the MIME type, or -1 if error
     */
    def findOrCreate(ts : String) : MimeTypeId = {
        cache get ts match {
            case Full(mt) ⇒ mt.getSafeKey
            case other : EmptyBox ⇒
                other match {
                    case _ : Failure ⇒ Log.error(s"error looking for MIME type: $ts")
                    case Empty ⇒
                }
                tryo(MimeType.create.mtstring(ts).saveMe()) match {
                    case Full(mt) ⇒
                        cache cache mt
                        mt.getSafeKey
                    case other : EmptyBox ⇒
                        other match {
                            case _ : Failure ⇒ Log.error(s"error creating MIME type: $ts")
                            case Empty ⇒
                        }
                        MimeTypeId(-1L)
                }
        }
    }
}
