/**
  * Copyright © 2012-2016 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.model

import choice.fs.ChoiceData
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

import scala.collection.mutable
import scala.ref.WeakReference

/**
 * A DataNode maintains a reference count to determine when it is no
 * longer needed. When the count goes to zero, the DataNode is deleted from
 * the database. See link() and unlink(). The reference count also indicates
 * when a DataNode is being shared by multiple Resources. An attempt to
 * write to a shared DataNode results in copy-on-write semantics.
 */
class DataNode extends LongKeyedMapper[DataNode] with IdPK with HasSafeKey[DataNodeId] {
    final def getSingleton : DataNode.type = DataNode

    object refcount extends MappedLong(this)
    object size extends MappedLong(this)
    object seqnum extends MappedLong(this)
    object hash extends MappedBytes(this, 16)

    /** Get the key for this entry wrapped as a DataNodeId */
    def getSafeKey : DataNodeId = DataNodeId(this.id.get)
    
    /**
     * Get the data associated with this DataNode as an array of bytes.
     *
     * @return	the data as bytes
     */
    def getBytes : Array[Byte] = ChoiceData.getBytes(seqnum.get)

    /**
     * Get the data associated with this DataNode as a string.
     *
     * @return	the data as a string
     */
    def getString : String = new String(getBytes)

    /**
     * Check whether this DataNode contains the specified data. If a positive
     * result is expected, the caller should specify "likely" as true. This
     * avoids calculating a hash value for the supplied content, because it
     * will likely match the hash value in the DataNode, and the DataNode data
     * will have to be retrieved for a confirming comparison. The default for
     * "likely" is false, which will compare hash values before retrieving the
     * DataNode data.
     *
     * @param content	the content in question
     * @param likely	true if a true result is expected, default false
     *
     * @return true if the content matches the DataNode data, otherwise false
     */
    def contains(content : Array[Byte], likely : Boolean = false) : Boolean = {
        if (likely || md5(content).sameElements(hash.get))
            content.sameElements(getBytes)
        else false
    }

    /**
     * Increment the reference count for this DataNode.
     *
     * @return	this DataNode
     */
    def link : DataNode = refcount(refcount.get + 1).saveMe()

    /**
     * Decrement the reference count for this DataNode. If it reaches zero,
     * delete this DataNode and its associated DataNodes from the database.
     *
     * @return	true if the data is deleted, else false
     */
    def unlink : Boolean = {
        val count = refcount.get - 1
        if (count <= 0) {
            ChoiceData.removeDataFile(seqnum.get)
            delete_!
        }
        else {
            refcount(count).saveMe()
            false
        }
    }
}

object DataNode extends DataNode with LongKeyedMetaMapper[DataNode] {
    override def dbIndexes : List[BaseIndex[DataNode]] = Index(hash) :: super.dbIndexes

    private[DataNode] val Log = Logger("choice.model.Data")

    private[DataNode] val nodes = mutable.Map[Long, WeakReference[DataNode]]()

    /**
     * This function should always be used to retrieve a DataNode by its key.
     * The purpose is to ensure that there is never more than one copy of each DataNode
     * in memory at once.
     *
     * Any DataNode located by direct access to the database should be entered
     * into the cache before being returned to a caller.
     *
     * This should operate as a write-through cache. Any changes to a DataNode
     * in the cache should be immediately forwarded to the database.
     *
     * @param key the id of the desired DataNode
     * @return the boxed DataNode
     */
    override def findByKey(key: Long): Box[DataNode] = synchronized {
        nodes get key flatMap (_.get) match {
            case Some(dnode) ⇒ Full(dnode)
            case None ⇒
                super.findByKey(key) map putDataNode
        }
    }

    /**
     * Put a DataNode in the cache of weak references. This should be called for any
     * DataNode that is returned to a caller. Functions receiving a DataNode argument
     * generally should be able to assume the DataNode is already cached.
     *
     * @param dnode the DataNode to be cached
     * @return the same DataNode
     */
    def putDataNode(dnode : DataNode) : DataNode = synchronized {
        val id = dnode.id.get
        if (id <= 0L) {
            Log.error(s"putDataNode: invalid id $id")
        }
        else {
            nodes put (id, WeakReference(dnode))
        }
        dnode
    }

    /**
      * Delete a DataNode, removing it from the cache. Optionally delete the associated
      * ChoiceData file.
      *
      * @param dnode the DataNode to be deleted
      * @param deleteData true if the associated ChoiceData file should be deleted as well
      * @return the result of dnode.delete_! or else false
      */
    def deleteDataNode(dnode : DataNode, deleteData : Boolean = true) : Boolean = synchronized {
        val id = dnode.id.get
        if (id <= 0L) {
            Log.error(s"deleteDataNode: invalid id $id")
            false
        }
        else {
            if (deleteData) {
                val seqnum = dnode.seqnum.get
                ChoiceData.removeDataFile(seqnum)
            }
            nodes remove id
            tryo(dnode.delete_!) openOr false
        }
    }

    /**
     * Make a DataNode for data with a given hash value. The reference count is initialized
     * to one. No attempt is made to find another DataNode with the same hash value.
     *
     * @param hash	the 16-byte hash value
     * @param data	the data to store
     * @return a boxed DataNode if successful, otherwise Failure
     */
    def make(hash : Array[Byte], data : Array[Byte]) : Box[DataNode] = {
        val (seqnum, length) = ChoiceData.makeDataFile(data)
        if (data.length != length) {
            val fail = Failure("length mismatch: content " + data.length +
                    			" bytes, file " + length + " bytes")
            Log.error(fail)
            fail
        }
        else tryo {
            val newdnode = DataNode.create.refcount(1).size(length).seqnum(seqnum).hash(hash).saveMe()
            putDataNode(newdnode)
        }
    }

    /**
     * Make a DataNode to store a given array of bytes. The hash
     * value of the bytes is calculated, and used to look for an existing DataNode that
     * already contains the specified data. If one is found, its referenced count is
     * incremented and it is returned. Otherwise a new DataNode is created with a
     * reference count of one, and the data is stored in it.
     *
     * @param content	the data to be stored
     * @return a boxed DataNode if successful, otherwise Failure
     * Note that Empty is returned if the content array is empty
     */
    def storeBytes(content : Array[Byte]) : Box[DataNode] = {
        val hash = md5(content)
        storeBytes(hash, content)
    }

    def storeBytes(hash : Array[Byte], content : Array[Byte]) : Box[DataNode] = {
        tryo(DataNode.findAll(By(DataNode.hash, hash))) match {
            case Full(list) ⇒ list.find(_.getBytes.sameElements(content)) match {
                case Some(dnode) ⇒
                    // We assume the database is up-to-date with any already cached
                    // instance of this DataNode.
                    Full(putDataNode(dnode).link)
                case None ⇒ make(hash, content)
            }
            case Empty ⇒ make(hash, content)
            case f : Failure ⇒ f
        }
    }

    /**
     * Find or create a DataNode for the data file with the given sequence number.
     * If a DataNode already exists for a data file that contains the
     * same data, increment its reference count and use it. In that case, the
     * data file given by the argument sequence number is deleted.
     *
     * If there is no DataNode for the data contained in the given file, one is created,
     * with its reference count initialized to one.
     * 
     * This assumes that if a DataNode with the same sequence number exists already,
     * it is referring to the same data as implied by argument sequence number.
     *
     * @param dataSeqnum	sequence number of a data file that is assumed not to
     * 						have an associated DataNode
     * @return a boxed DataNode associated with the data
     */
    def storeData(dataSeqnum : Long) : Box[DataNode] = {
        // There is no data if the sequence number isn't strictly positive
        if (dataSeqnum <= 0) Empty
        else DataNode.find(By(DataNode.seqnum, dataSeqnum)) or {
            // Read the file associated with the given sequence number, and get its
            // hash value and size.
            val (hash, size) = ChoiceData.getHashAndSize(dataSeqnum)

            // Now look for DataNodes with the same hash and length
            val otherdn = tryo(DataNode.findAll(By(DataNode.hash, hash), By(DataNode.size, size))) match {
                case Full(list) ⇒
                    // See if any match the data we just stored
                    list.find { dn ⇒ ChoiceData.compareFiles(dataSeqnum, dn.seqnum.get) == 0 } match {
                        case Some(dn) ⇒ Full(dn)
                        case None ⇒ Empty
                    }
                case e : EmptyBox ⇒
                    Log.error("storeData", e)
                    e
            }
            otherdn match {
                case Full(dn) ⇒
                    putDataNode(dn)
                    // Duplicate data file already exists, remove the one passed in
                    // (if it's not the same one), and link to the duplicate.
                    if (dataSeqnum != dn.seqnum.get) ChoiceData.removeDataFile(dataSeqnum)
                    dn.link
                    Full(dn)
                case _ : EmptyBox ⇒
                    tryo {
                        putDataNode(DataNode.create
                            .refcount(1).size(size).seqnum(dataSeqnum).hash(hash).saveMe())
                    }
            }
        }
    }
    
    def exists(id : Long) : Boolean = DataNode.findByKey(id) match {
        case Full(_) ⇒ true
        case _ : EmptyBox ⇒ false
    }

    /**
     * Get the data for a DataNode, given its id value.
     *
     * @param id	primary key of a DataNode
     *
     * @return	the data as a boxed array of bytes, or Failure if the specified
     * 			DataNode does not exist
     */
    def getBytes(id : Long) : Box[Array[Byte]] = {
        DataNode.findByKey(id) match {
            case Full(dnode) ⇒ Full(dnode.getBytes)
            case e : EmptyBox ⇒ e ?~! ("DataNode " + id + " not found")
        }
    }

    /**
     * Get the data for a DataNode, given its id value.
     *
     * @param id	primary key of a DataNode
     *
     * @return	the data as a boxed string, or Failure if the specified
     * 			DataNode does not exist
     */
    def getString(id : Long) : Box[String] = {
        DataNode.findByKey(id) match {
            case Full(list) ⇒ Full(list.getString)
            case e : EmptyBox ⇒ e ?~! ("DataNode " + id + " not found")
        }
    }

    def link(id : Long) : Box[DataNode] = {
        DataNode.findByKey(id) match {
            case Full(list) ⇒ Full(list.link)
            case e : EmptyBox ⇒ e ?~! ("DataNode " + id + " not found")
        }
    }

    def unlink(id : Long) : Box[Boolean] = {
        DataNode.findByKey(id) match {
            case Full(list) ⇒ Full(list.unlink)
            case e : EmptyBox ⇒ e ?~! ("DataNode " + id + " not found")
        }
    }
}
