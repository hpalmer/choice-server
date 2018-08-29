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
  * Plain file support.
  *
  * @author Howard Palmer
  */
package choice.fs

import java.io._
import java.nio.file.{Path, StandardOpenOption}

import choice.access.{AnyRightsCheck, Principal, RightDef, RightsCheck}
import choice.actor.DbManager
import choice.fs.vfs.{CanStreamInput, CanStreamOutput}
import choice.lib.ExtendedBox._
import choice.model.{MimeTypeId, Resource}
import net.liftweb.common.{Full, _}
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers._
import net.liftweb.util.TimeHelpers.millis

/**
 * This defines a Vnode for any file which is not special, that is, which does not
 * have a MimeTypeHandler associated with its MIME type.
 *
 * @param resource the DB Resource entry for this file
 */
class CfsPlainNode(resource : Resource) extends CfsVnode(resource) {

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * some subclass of VFile that references this Vnode. Using the VFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * @param principal the principal to be associated with the VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[CfsPlain] = {
        Full(new CfsPlain (path, principal, this))
    }

    /**
     * Change the MIME type of the file.
     *
     * It is not permitted to change the MIME type to a type which would not be a
     * CfsPlain file.
     *
     * @param mimetype the new MIME type
     * @return the new MIME type, boxed
     */
    def setMimeType(mimetype : String) : Box[String] = withWriteLock { () ⇒
        DbManager getMimeTypeHandler mimetype flatMap { mth ⇒
            if (mth.isSpecial_?) Failure(s"not allowed: $mimetype is special")
            else {
                val mtid = DbManager getMimeTypeId mimetype
                if (mtid == MimeTypeId(-1L)) Failure(s"failed to assign id to MIME type $mimetype")
                else {
                    getResource setMimeType mtid map (_ ⇒ mimetype)
                }
            }
        }
    }

    def getInputData : Box[(Long, Path)] = {
        val seqnum = getResource.getSeqnum
        if (seqnum <= 0L) Empty
        else Full((seqnum, ChoiceData getPath seqnum))
    }

    /**
      * Ensure that this file is associated with a ChoiceData file that is not
      * shared with any other files.
      *
      * @return a box containing the ChoiceData file sequence number, and its
      *         NIO Path
      */
    def getUniqueData : Box[(Long, Path)] = {
        val snbox = withWriteLock { () ⇒
            val seqnum = resource.getSeqnum
            // If there is a DataNode for this file, it might be shared with
            // other files. If the sequence number from the Resource is not
            // strictly positive, there is no associated ChoiceData file.
            // In either case, let DbManager handle it, so that any changes
            // are properly serialized.
            if (resource.getDataNodeId.isDefined || seqnum <= 0) {
                DbManager makeUniqueData this
            }
            else Full(seqnum)
        }
        snbox flatMap { seqnum ⇒
            val path = ChoiceData getPath seqnum
            Full((seqnum, path))
        }
    }

    /**
     * Get a RandomAccessFile for this file.
     *
     * If the file has no ChoiceData file, one is created. If writeAccess is specified and
     * the ChoiceData file is shared by multiple Resources, a copy is made.
     *
     * @param mode file mode (see ChoiceData getRandomAccessFile)
     * @return a box containing the ChoiceData file sequence number and the RandomAccessFile
     */
    def getRandomAccessFile(mode : String) : Box[(Long, RandomAccessFile)] = {
        val snbox = withWriteLock { () ⇒
            val seqnum = getResource.getSeqnum
            // If makeUniqueData has been called already, the Resource seqnum will be
            // strictly positive, and there will be no DataNode associated with the
            // Resource. If the mode is "r", there is no need to makeUniqueData, unless
            // there is no ChoiceData file associated with the Resource.
            if (seqnum <= 0 || (mode != "r" && resource.getDataNodeId.isDefined)) {
                DbManager makeUniqueData this
            }
            else Full(seqnum)
        }
        snbox flatMap { seqnum ⇒
            ChoiceData getRandomAccessFile (seqnum, mode) map ((seqnum, _))
        }
    }

    override def isSpecial_? : Boolean = false
}

class CfsPlain(path : CfsAbsolutePath, principal : Principal, vnode : CfsPlainNode)
    extends CfsFile(path, principal, vnode) with CanStreamInput with CanStreamOutput {

    private var _rafhandles = List.empty[RandomAccessFile]

    override def getVnode : CfsPlainNode = vnode

    /**
     * Check whether this file is special, which is true if it has an associated
     * MimeTypeHandler.
     *
     * @return false since this is a plain file
     */
    override def isSpecial_? : Boolean = false

    /**
     * Change the MIME type of this file.
     *
     * @param mimetype the new MIME type
     * @return the new MIME type, boxed
     */
    def setMimeType(mimetype : String) : Box[String] = (CfsFile canSetFileInfo this){ () ⇒
        getVnode setMimeType mimetype
    }

    def replaceFileData(data : String) : Box[CfsFile] = (CfsPlain canGetOutputStream this) { () ⇒
        val (dataSeqnum, _) = ChoiceData makeDataFile data.getBytes
        val vnode = getVnode
        vnode withWriteLock { () ⇒
            DbManager replaceData (getVnode, dataSeqnum, millis) map (_ ⇒ this)
        }
    }

    /**
      * Return the sequence number and path of the ChoiceData file associated with this
      * file, if any. Return Empty if not.
      *
      * @return a box containing the ChoiceData sequence number and NIO Path of the
      *         ChoiceData file
      */
    def getInputData : Box[(Long, Path)] = (CfsPlain canGetInputStream this) { () ⇒
        getVnode.getInputData
    }

    /**
      * Ensure this file is associated with an unshared ChoiceData file, in preparation
      * for writing to it.
      *
      * @return a box containing the ChoiceData sequence number and NIO Path of the
      *         ChoiceData file
      */
    def getUniqueData : Box[(Long, Path)] = (CfsPlain canGetOutputStream this) { () ⇒
        getVnode.getUniqueData
    }

    /**
      * Note that CfsFiles.newInputStream will call getInputData(), which checks access
      * control.
      *
      * @return a boxed input stream if successful. A file that does not support
      *         input streams may return a zero-length stream, Empty, or Failure.
      */
    override def getInputStream : Box[InputStream] = tryo(CfsFiles.newInputStream(this))

    /**
      * Note that CfsFiles.newOutputStream will getUniqueData(), which checks access control.
      *
      * @param append true if data written to the stream should be appended to
      *               any existing data
      * @param exclusive true if exclusive access to write the file is desired
      * @return a boxed output stream if successful.
      *         A file that does not support output streams normally should return
      *         a Failure
      */
    override def getOutputStream(append : Boolean, exclusive : Boolean) : Box[OutputStream] = {
        val options =
            if (append) {
                Seq(StandardOpenOption.APPEND, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
            } else Seq()
        tryo(CfsFiles.newOutputStream(this, options : _*))
    }

    def getRandomAccessFile(mode : String) : Box[(Long, RandomAccessFile)] = {
        {
            if (mode != "r") (CfsPlain canGetOutputStream this) { () ⇒
                (CfsPlain canGetInputStream this) (() ⇒ getVnode getRandomAccessFile mode)
            }
            else (CfsPlain canGetInputStream this) (() ⇒ getVnode getRandomAccessFile mode)
        } use { pair ⇒
            _rafhandles = pair._2 :: _rafhandles
        }
    }

    /**
     * Close the handle. This will also close any open input or output streams,
     * and release any other resources associated with the VFile.
     */
    override def close() : Unit = {
        tryo(_rafhandles foreach (_.close()))
        _rafhandles = Nil
        super.close()
    }
}

object CfsPlain extends MimeTypeHandler {
    override protected val Log = Logger("choice.fs.CfsPlain")

    override val getName = "Plain File Type"

    override val getSchemas : List[BaseMetaMapper] = Nil

    override val getMimeType = "*/*"

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override val getRights : List[RightDef] = List(
        RightDef("create_file", "plain file", "create a file"),
        RightDef("unlink_file", "plain file", "unlink a file"),
        RightDef("read_file", "plain files", "read the file, its attributes, and metadata"),
        RightDef("write_file", "plain files", "write the file, its attributes, and metadata"),
        RightDef("delete_file", "plain files", "delete the file"),
        RightDef("write_file_meta", "plain files", "write the file metadata, but not the file"),
        RightDef("list_metadata", "plain files", "list metadata files")
    )

    /**
     * Check whether a Resource has the MIME type associated with this handler.
     */
    override def isType_?(mtid : MimeTypeId) : Boolean = {
        (dbPlugin dbmGetMimeTypeHandler mtid) map (_ == this) openOr false
    }

    override def isType_?(resource : Resource) : Boolean = isType_? (resource.getMimeTypeId)

    override def isContainer_? = false

    override def isSpecial_? = false

    override def instantiate(resource : Resource) : Box[CfsPlainNode] = {
        if (isType_?(resource)) Full(new CfsPlainNode(resource))
        else Failure(s"resource id ${resource.getSafeKey.id} is a special type")
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_file")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_file")

    val canGetInputStream = AnyRightsCheck("read_file")
    val canGetOutputStream = AnyRightsCheck("write_file")
    val canListMetadata = AnyRightsCheck("list_metadata")
}
