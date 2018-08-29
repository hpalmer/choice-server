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
  * @author Howard Palmer
  */
package choice.actor

import _root_.net.liftweb._
import common._
import mapper._
import util._
import Helpers._

import scala.collection.mutable

trait CacheStatistics {
    var avgHitTime : Double = 0
    var avgMissTime : Double = 0
    var hitCount : Long = 0
    var missCount : Long = 0

    /**
     * Record a cache hit, computing a running average of the processing time of a hit.
     *
     * @param ns System.nanoTime() at the start of hit processing
     */
    def hit(ns : Long) : Unit = {
        val duration = System.nanoTime() - ns
        hitCount += 1
        val weight = (hitCount + missCount).toDouble
        avgHitTime = ((weight - 1) * avgHitTime + duration.toDouble) / weight
    }

    def miss(ns : Long) : Unit = {
        val duration = System.nanoTime() - ns
        missCount += 1
        val weight = (hitCount + missCount).toDouble
        avgMissTime = ((weight - 1) * avgMissTime + duration.toDouble) / weight
    }

    def clearStatistics() : Unit = {
        hitCount = 0L
        missCount = 0L
        avgHitTime = 0.0
        avgMissTime = 0.0
    }

    def accessCount : Long = hitCount + missCount

    def hitFrequency : Double = {
        val accesses = hitCount + missCount
        if (accesses == 0) 1.0
        else hitCount.toDouble / accesses.toDouble
    }

    def missFrequency : Double = 1.0 - hitFrequency

    def missFactor : Double = if (avgHitTime == 0.0) Double.PositiveInfinity else avgMissTime/avgHitTime

    def efficacy : Double = hitFrequency * scala.math.log(missFactor)

    def getStatistics : Map[String, Any] = Map("accesses" → accessCount, "hits" → hitCount, "misses" → missCount,
                                              "hitfreq" → hitFrequency, "missfreq" → missFrequency,
                                              "hittime" → avgHitTime, "misstime" → avgMissTime,
                                              "missfactor" → missFactor, "efficacy" → efficacy)
}

/**
 * Base class for an element or a list head of a doubly-linked list.
 * @tparam T the type of the element
 */
class QueueLink[T <: QueueLink[T]] {
    self : T ⇒
    var next : T = this
    var prev : T = this
    def insertAfter(elem : T) : T = {
        elem.prev = this
        elem.next = next
        next.prev = elem
        next = elem
        elem
    }
    def insertBefore(elem : T) : T = {
        elem.prev = prev
        elem.next = this
        prev.next = elem
        prev = elem
        elem
    }
    def remove() : Unit = {
        prev.next = next
        next.prev = prev
    }
}

/**
 * Base class for an LRU cache. The maximum number of elements to be held in the cache
 * can be set. Subclasses define a function to fetch an element that is not found in
 * the cache. That function is called on a cache miss, and the returned element is
 * added to the cache. Subclasses also define a function that is called when an
 * element is removed from the cache.
 */
abstract class LRUCacheBase[K, T] extends Iterable[(K, T)] with CacheStatistics {

    /** Base class for both the list head and the list elements */
    protected abstract class EntryBase extends QueueLink[EntryBase]

    /** List head class */
    protected case class EntryHead() extends EntryBase

    /** List element class */
    protected case class Entry(key : K, value : T) extends EntryBase

    /** List of entries in the hash table */
    protected var lruHead : EntryHead = EntryHead()

    /** Hash table of entries */
    protected val lruMap = new mutable.HashMap[K, Entry]

    /** Current number of entries */
    protected var _count : Int = 0

    /** Limit on number of entries. Negative indicates no limit */
    protected var _limit : Int = -1

    /** On a cache miss, return the element with the given unique id */
    def fetch(id : K) : Box[T]
    
    /** Cache the element that has the specified id */
    def cache(key : K, value : T) : Unit = {
        // Look for element with the given key
        lruMap get key match {
            case None ⇒
                // No such element. Add a new one.
                val entry = Entry(key, value)
                lruMap put (key, entry)
                lruHead insertAfter entry
                _count += 1
                enforceLimit()
            case Some(entry) ⇒
                // Does the entry already have the desired value?
                if (entry.value == value) {
                    // Yes, just move it to the head of the list
                    entry remove ()
                    lruHead insertAfter entry
                }
                else {
                    // Otherwise replace the entry in the hash table
                    val nentry = Entry(key, value)
                    lruMap update (key, nentry)
                    // Remove the replaced entry from the list
                    entry remove ()
                    // Put the new entry at the head of the list
                    lruHead insertAfter nentry
                }
        }
    }

    /**
     * Evict the element that has specified id, if it is in the cache. This does not
     * check whether canEvict() is true, and does not call evicted().
     * 
     * @param key unique key of element
     * 
     * @return true if the element was in the cache
     */
    def evict(key : K) : Boolean = {
        lruMap remove key match {
            case None ⇒ false
            case Some(entry) ⇒
                // Remove the element from the list
                entry remove ()
                _count -= 1
                true
        }
    }
    
    /**
     * Return the element with the specified unique id. If the element is not in the cache,
     * it is fetched and cached.
     * 
     * @param key unique key of element
     * 
     * @return the boxed element
     */
    def get(key : K) : Box[T] = {
        val now = System.nanoTime()
        lruMap get key match {
            case None ⇒
                // Element not present, so fetch it
                val result = fetch(key) match {
                    case full @ Full(elem) ⇒
                        cache(key, elem)
                        full
                    case e : EmptyBox ⇒ e
                }
                miss(now)
                result
            case Some(entry) ⇒
                // ELement present. Move to head of the list if it's not already.
                if (lruHead.next ne entry) {
                    entry.remove()
                    lruHead insertAfter entry
                }
                hit(now)
                Full(entry.value)
        }
    }

    /**
     * Try to get an element from the cache, given its key, but do not fetch the element
     * if it isn't present.
     *
     * @param key unique key of element
     * @return the element as an option, None if not present in the cache
     */
    def rawget(key : K) : Option[T] = {
        val now = System.nanoTime()
        lruMap get key match {
            case None ⇒
                miss(now)
                None
            case Some(entry) ⇒
                hit(now)
                Full(entry.value)
        }
    }

    /**
     * Check whether an entry can be evicted. For example, there may be outstanding
     * references to the entry. By default, entries can always be evicted, but a
     * subclass can override this.
     *
     * @param key the entry key
     * @param value the entry value
     * @return true if the entry can be evicted
     */
    def canEvict(key : K, value : T) : Boolean = true

    /**
     * This is called when an entry is evicted due to a limit on the number of entries.
     * It is not called when an entry is explicitly evicted by calling evict().
     *
     * @param key the entry key
     * @param value the entry value
     */
    def evicted(key : K, value : T) : Unit = {}

    /** Return a list of entries in LRU order */
    protected def toLRUEntryList : List[Entry] = {
        var list : List[Entry] = Nil
        var prev = lruHead.prev
        while (prev ne lruHead) {
            prev match {
                case entry : Entry ⇒
                    list = entry :: list
                    prev = prev.prev
                case _ ⇒
            }
        }
        list
    }

    /** Return a list of entries in MRU order */
    protected def toMRUEntryList : List[Entry] = {
        var list : List[Entry] = Nil
        var next = lruHead.next
        while (next ne lruHead) {
            next match {
                case entry : Entry ⇒
                    list = entry :: list
                    next = next.next
                case _ ⇒
            }
        }
        list
    }

    def MRUIterator : Iterator[(K, T)] = {
        var _next = lruHead.next
        new Iterator[(K, T)] {
            override def hasNext: Boolean = _next ne lruHead

            override def next(): (K, T) = {
                val result = _next match {
                    case Entry(key, value) ⇒ (key, value)
                    case _ ⇒ null
                }
                _next = _next.next
                result
            }
        }
    }

    def LRUIterator : Iterator[(K, T)] = {
        var _prev = lruHead.prev
        new Iterator[(K, T)] {
            override def hasNext: Boolean = _prev ne lruHead

            override def next(): (K, T) = {
                val result = _prev match {
                    case Entry(key, value) ⇒ (key, value)
                    case _ ⇒ null
                }
                _prev = _prev.prev
                result
            }
        }
    }

    def iterator : Iterator[(K, T)] = MRUIterator

    /** Return list of (key, value) pairs in LRU order */
    def toLRUList : List[(K, T)] = toLRUEntryList map (entry ⇒ (entry.key, entry.value))

    /** Return list of (key, value) pairs in LRU order */
    def toMRUList : List[(K, T)] = toMRUEntryList map (entry ⇒ (entry.key, entry.value))

    /** Return the current limit on the maximum number of cache elements */
    def getLimit : Int = _limit
    
    /** Set a new limit on the maximum number of cache elements */
    def setLimit(newlimit : Int) : Int = {
        val prevLimit = _limit
        _limit = newlimit
        if (_limit < prevLimit) {
            enforceLimit()
        }
        clearStatistics()
        _count
    }

    /** Enforce the cache size limit. Return the current size. */
    protected def enforceLimit() : Int = {
        if (_limit >= 0) {
            var prev = lruHead.prev
            while ((prev ne lruHead) && (_count > _limit)) {
                val tprev = prev.prev
                prev match {
                    case entry@Entry(key, value) ⇒
                        if (canEvict(entry.key, entry.value)) {
                            entry remove ()
                            lruMap remove key
                            _count -= 1
                            evicted(key, value)
                        }
                    case _ ⇒
                }
                prev = tprev
            }
        }
        _count
    }
}

/**
 * LRU cache for items in a specified database table.
 * 
 * @param singleton the singleton object for the database table
 */
abstract class DbTableCache[K, T <: LongKeyedMapper[T] with IdPK](singleton : T with LongKeyedMetaMapper[T])
    extends LRUCacheBase[K, T] {

    /**
     * Fetch the element with the given key from the database. The key can
     * contain anything which is sufficient to construct a query that will
     * return a unique table element. However, this function probably does
     * not guarantee the uniqueness.
     * 
     * @param key the key to use to find the element in the database
     * @return the boxed entry corresponding to the key
     */
    def fetch(key : K) : Box[T]

    /**
     * Cache an element if it's not already cached.
     *
     * @param elem the element to be cached
     */
    def cache(elem : T) : Unit

    /** Write an element to the database, and cache the result */
    def save(elem : T) : Box[T] = tryo(elem.saveMe()) match {
        case full @ Full(e) ⇒
            cache(e)
            full
        case e : EmptyBox ⇒ e
    }
}
