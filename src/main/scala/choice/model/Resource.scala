/**
  * Copyright © 2012-2018 The Board of Trustees of The Leland Stanford Junior University.
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
  * Access to Resource table in database.
  *
  * @author Howard Palmer
  */
package choice.model

import java.io.{InputStream, RandomAccessFile}

import choice.access.CfsPrincipal
import choice.core.CacheFilter
import choice.fs._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

/**
 * A resource is file object stored in the database. Each resource is identified by
 * its unique id. A Resource table entry contains metadata for the resource, such as
 * its creation time, MIME type, and who created it. The actual file data is stored
 * as a DataNode referenced by the Resource.
 * 
 * Regular files, folders, and symbolic links are represented by a resource.
 * For a folder, the Resource captures its metadata, but the containment information
 * for resources in a folder is represented by FsName table entries.
 */
class Resource extends LongKeyedMapper[Resource] with IdPK with HasSafeKey[ResourceId] {
    
    import Resource.Log
    
	final def getSingleton : Resource.type = Resource

	/** The user resource of the user who owns this resource */
	object owner extends MappedLongForeignKey(this, Resource)
    
	/**
	 * A Resource can be referenced by multiple FsName entries, since it can appear
	 * in multiple folders with independent names. This corresponds to a hard link
	 * in a regular filesystem. This count includes the number of FsName entries
     * that reference the Resource as a member of a container. It does not include
     * the count of FsName entries that reference this resource as the container.
     * Some other references to a Resource by its id may exist in the system, and
     * those may also be included in this count in cases where they should prevent
     * the Resource from being deleted.
	 */
	object refcount extends MappedLong(this)

    /**
     * This is the size in bytes of the data associated with the resource. It
     * should match the size in any referenced DataNode.
     */
	object size extends MappedLong(this)
	
	/**
	 * The creation time of this Resource, in milliseconds since the epoch.
	 */
	object crtime extends MappedLong(this)

    /**
     * The last modification time of this Resource, in milliseconds since the epoch.
     */
    object mtime extends MappedLong(this)

	/**
	 * The MIME type id of this Resource.
	 */
	object mtid extends MappedLongForeignKey(this, MimeType)
	
	/**
	 * Id of a resource represented in another table, such as a UserGroup
	 * or UserReg.
	 */
	object altid extends MappedLong(this)
	
	/**
	 * The data associated with this Resource. Note that multiple Resources may
	 * reference the same DataNode, and may not contain the same metadata. The
	 * semantics of this sharing is copy-on-write. That is, if more than one
	 * Resource references a DataNode, any change to the data results in a copy
	 * to a new DataNode.
	 */
	object dnode extends MappedLongForeignKey(this, DataNode) {
	    override def dbNotNull_? = false

        /**
         * The base class implementation of this method keeps an internal
         * reference to a private copy of the DataNode. Although DataNodes
         * are almost immutable, their reference counts are updated as
         * Resources link and unlink them. If two Resources were in DbManager's
         * cache with private copies of the same DataNode, the reference count
         * could be changed in one copy, but would not be updated in the other.
         * If the reference count in the second copy were then changed, the
         * reference count would no longer be correct. This is true even when
         * reference count updates are serialized (which they are presumably,
         * in DbManager).
         *
         * @return a boxed, shared copy of the referenced DataNode
         */
        override def obj: Box[DataNode] = getDataNode
    }

    /**
     * This is a copy of the seqnum field from the referenced DataNode, if any.
     * It is duplicated here so that the ChoiceData file can be located without
     * pulling in the DataNode.
     */
    object seqnum extends MappedNullableLong(this)

    /** Get the key for this entry wrapped as a ResourceId */
    def getSafeKey : ResourceId = ResourceId(this.id.get)
    
    def getOwnerId : ResourceId = ResourceId(this.owner.get)
    
    def getMimeTypeId : MimeTypeId = MimeTypeId(this.mtid.get)

    def getRefCount : Long = refcount.get

    def getDataNodeId : Box[DataNodeId] = {
        val id = dnode.get
        if (id <= 0L) Empty
        else Full(DataNodeId(id))
    }

    def getDataNode : Box[DataNode] = {
        val id = dnode.get
        if (id <= 0L) Empty
        else DataNode.findByKey(id)
    }

    /**
     * Return the sequence number of the associated ChoiceData file, if there is one.
     * Otherwise return -1.
     *
     * @return ChoiceData file sequence number or -1
     */
    def getSeqnum : Long = seqnum.get openOr -1L

    def getSize : Long = this.size.get

    def getCreation : TimeValue = TimeValue(this.crtime.get)

    def getLastModified : TimeValue = TimeValue(this.mtime.get)

    def getAlternateId : Option[Long] = if (altid.get == 0L) None else Some(altid.get)

    def asMap : Map[String, Any] = {
        val owner = CfsPrincipal(getOwnerId)
        Map("id" → getSafeKey.id,
            "owner" → owner.getPrincipalName,
            "ownerId" → owner.getPrincipalId.id,
            "refcount" → getRefCount,
            "crtime" → getCreation.msec,
            "mtime" → getLastModified.msec,
            "mimetype" → getMimeType,
            "size" → getSize,
            "altid" → getAlternateId)
    }

	def getBytes : Box[Array[Byte]] = {
	    getDataNode.map(_.getBytes)
	}
	
	def getString : Box[String] = {
	    getDataNode.map(_.getString)
	}

	def getInputStream : Box[InputStream] = ChoiceData getInputStream getSeqnum

    def getRandomAccessFile(mode : String) : Box[RandomAccessFile] = ChoiceData getRandomAccessFile (getSeqnum, mode)

    /**
     * Get the MIME type string for this resource. An entry in the MimeType table should
     * exist, but if not, "application/octet-stream" is returned.
     *
     * @return	MIME type string
     */
    def getMimeType : String = MimeType get MimeTypeId(mtid.get) match {
        case Full(mt) ⇒ mt.mtstring.get
        case other : EmptyBox ⇒
            other match {
                case _ : Failure ⇒ Log.error(s"MIME type not found for resource id: ${this.id}")
                case Empty ⇒
            }
            "application/octet-stream"
    }

    def remove() : Boolean = {
        getDataNode match {
            case Full(dn) ⇒ dn.unlink
            case Empty ⇒
            case f : Failure ⇒ Log.error(s"error removing DataNode id ${dnode.get}", f)
        }
        this.delete_!
    }

    /**
     * Increment the reference count of this resource. Typically this is done when
     * the resource is linked to a container, or when the resource represents a
     * container to which a link has been added. The new reference count is returned,
     * boxed.
     */
    def addReference() : Box[Long] = tryo {
        val count = refcount.get + 1
        refcount(count).saveMe()
        count
    }

    /**
     * Decrement the reference count of this resource. The count is not allowed to
     * go negative. The resource is not removed (here) when the count goes to zero.
     */
    def removeReference() : Box[Long] = tryo {
        val count = refcount.get - 1
        if (count < 0) {
            sys.error(s"resource id ${id.get}: attempt to make reference count negative")
        }
        else {
            refcount(count).saveMe()
            count
        }
    }

    def setMimeType(mtid : MimeTypeId) : Box[Resource] = {
        tryo(this.mtid(mtid).saveMe())
    }

    def setOwner(principal : ResourceId) : Box[Resource] = {
        tryo(this.owner(principal.id).saveMe())
    }

    def setSeqnum(newseq : Long) : Box[Resource] = tryo(this.seqnum(if (newseq > 0) Full(newseq) else Empty).saveMe())

    def replaceData(data : String) : Resource = {
        replaceData(data.getBytes)
    }

    def replaceData(data : Array[Byte], mtime : Long = System.currentTimeMillis) : Resource = {
        CacheFilter remove getSafeKey
        val (dataSeqnum, _) = ChoiceData.makeDataFile(data)
        replaceData(dataSeqnum, mtime)
    }

    def replaceData(dataSeqnum : Long, mtime : Long) : Resource = {
        CacheFilter remove getSafeKey
        val oldDataNodeId = getDataNode map (_.id.get)
        val newDnode = DataNode.storeData(dataSeqnum)
        val resbox = newDnode match {
            case Full(nnode) ⇒
                // 2015-05-11 seqnum fields observed out-of-sync between Resource and DataNode
                // Unable to replicate, but now set the Resource seqnum unconditionally and
                // check for the condition.
                this.seqnum(Full(nnode.seqnum.get))
                val result = tryo(this.size(nnode.size.get).dnode(nnode).mtime(mtime).saveMe())
                this.seqnum.get match {
                    case Empty ⇒
                        Log.error(s"resource ${this.id.get} is missing seqnum ${nnode.seqnum.get}")
                    case Full(seqno) ⇒
                        if (seqno != nnode.seqnum.get) {
                            Log.error(s"resource ${this.id.get} has different seqnum $seqno (not ${nnode.seqnum.get})")
                        }
                    case _ : Failure ⇒
                        Log.error(s"resource ${this.id.get} has failure for seqnum")
                }
                // If there was already a DataNode for this Resource, unlink it. Even if the new
                // DataNode happens to be the same one, it's still correct because DataNode.storeData
                // will have bumped its reference count. However, it is necessary to re-fetch the old
                // DataNode, since it's reference count will have changed if it is the same as the new one.
                // In any case, the old DataNode should almost always already be in the cache.
                oldDataNodeId foreach { id ⇒
                    DataNode.findByKey(id) foreach (_.unlink)
                }
                result
            case Empty ⇒
                tryo(this.size(0L).dnode(Empty).mtime(mtime).seqnum(Empty).saveMe())
            case f : Failure ⇒ f
        }
        resbox match {
            case Full(res) ⇒ res
            case e : EmptyBox ⇒
                Log.error("replaceData failed", e)
                this
        }
    }
}

object Resource extends Resource with LongKeyedMetaMapper[Resource] {
    
    private[Resource] object Log extends Logger

    /**
     * Store a new Resource. The resource reference count is initially one, on the
     * assumption that the Resource will be linked to a filename. If this does not
     * happen, the caller is responsible for ensuring that the Resource is deleted.
     *
     * A Resource can be linked under multiple filenames, using FsName entries. The
     * reference count tracks how many such FsName entries exist for the Resource.
     *
     * If a ChoiceData sequence number is specified, it will be passed to the
     * DataNode manager for processing. It will either create a new DataNode entry
     * for the ChoiceData file, or return a existing DataNode entry for a ChoiceData
     * file containing the same data. If the latter case, the ChoiceData file
     * referenced by the original dataSeqnum is removed, as part of the DataNode
     * implementation of data deduplication.
     *
     * @param owner the Resource id of the owner of this Resource
     * @param mimetype the MIME type id
     * @param dataSeqnum the sequence number of an associated ChoiceData file, if any,
     *                   otherwise zero
     * @param ctime the creation timestamp, defaulting to the current time
     * @param altid the id of a related entry in some other database table, if any,
     *              otherwise defaulting to zero
     * @return the boxed Resource or a Failure
     */
    def storeResource(owner : Long,
            		  mimetype : Long,
                      dataSeqnum : Long,
                      ctime : Long = System.currentTimeMillis(),
                      altid : Long = 0) : Box[Resource] = {
        val res = Resource.create
            .owner(owner)
            .refcount(1L)
            .crtime(ctime)
            .mtime(ctime)
            .mtid(mimetype)
            .altid(altid)
        DataNode.storeData(dataSeqnum) match {
            case Full(dn) ⇒
                res.dnode(dn).size(dn.size.get).seqnum(Full(dn.seqnum.get))
                tryo(res.saveMe())
            case Empty ⇒
                res.dnode(Empty).size(0)
                tryo(res.saveMe())
            case f : Failure ⇒
                Log.error("failed to create resource", f)
                f
        }
    }
}
