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
  * Actor to serialize low-level filesystem changes.
  *
  * @author Howard Palmer
  */
package choice.actor

//import java.io.PrintStream
import java.nio.file.{Files, StandardCopyOption}

import choice.access.{BootPrincipal, Principal}
import choice.actor.DbManagerActor.{MakeUniqueDataRequest, UncacheVnodeRequest, ValidateCacheRequest, ValidateCacheResponse}
import choice.attributes.{AttrDef, AttrDefNode, AttrVal}
import choice.core.CacheFilter
import choice.fs._
import choice.fs.vfs.Vnode
import choice.model._
import net.liftweb.actor.LiftActor
import net.liftweb.common._
import net.liftweb.http.ListenerManager
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * This trait defines the interface to the DBManager actor.
 */
trait DbManager {
    // Import DbManager actor message types
    import choice.actor.DbManagerActor.{
        ClearFsNameCacheRequest, ClearVNodeCacheRequest,
        CloseRequest, CreateFileRequest, DereferenceRequest, FindContainersRequest, FindContainersResult,
        FindPathRequest, FindPathResult, GetRootRequest, InitializeDbManager, LinkRequest, LookupByIdRequest,
        LookupMembersRequest, LookupMembersResult, LookupRequest, MemberNameRequest, MembersRequest, MembersResult,
        ReferenceRequest, RegisterMimeTypeHandler, ReplaceDataRequest, ReviewCache, SetFsNameCacheLimitRequest,
        SetVNodeCacheLimitRequest, UnlinkRequest, UnlinkResponse}

    def Log : Logger

    /**
     * Get a root container. The default root container is named "/". The
     * dynamic reference count for the root Vnode is incremented, so the
     * caller should take some action to release it.
     *
     * @param name an optional name for an alternative root
     * @return a CfsFnode for the named root
     */
    def getRoot(name : String = "/") : CfsFnode = {
        DbManagerActor !! GetRootRequest(name) match {
            case Full(root : CfsFnode) ⇒ root
            case _ ⇒ null
        }
    }

    /**
     * Handle aspects of closing a file handle that referenced a specified Vnode.
     * This is called when the Vnode reference count is decremented to zero when
     * it is released by the file handle. This gives DbManager the opportunity to
     * remove the Vnode from its cache, but more importantly, if all the links to
     * the file have been removed from the filesystem, it is the point where the
     * file is actually deleted.
     *
     * Note that the Vnode reference count can be incremented again after a thread
     * has called this method, so DbManager must check it again. There is no race
     * condition as long as all cases where a Vnode reference count is incremented
     * from zero to one are serialized through DbManager. Specifically there is no
     * race when an open file handle increments the count, because the count should
     * never be zero when there is a open file handle for the Vnode.
     *
     * One might expect a write lock would be needed for this operation, but it is
     * not, since an external thread would need to first increment the Vnode
     * reference count from zero to one, and that would be serialized through
     * DbManager.
     *
     * @param vnode a Vnode which had its reference count decremented to zero
     */
    def close(vnode : CfsVnode) : Unit = {
        DbManagerActor !! CloseRequest(vnode)
    }

    /**
     * Lookup a name in a container, which the caller is assumed to have acquired.
     * A CfsVnode for the named file is returned if it is found. If successful, the
     * container is released and the returned Vnode is acquired. If unsuccessful,
     * the container remains acquired.
     *
     * @param container the container CfsVnode
     * @param name the name to find
     * @return a boxed CfsVnode for named file, or Empty
     */
    def lookup(container : CfsVnode, name : String) : Box[CfsVnode] = {
        assert(container.isReferenced_?)
        DbManagerActor !! LookupRequest(container, name) match {
            case Full(member : CfsVnode) ⇒
                assert(member.isReferenced_?)
                Full(member)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from LookupRequest")
        }
    }

    /**
     * Lookup a file by its file id. An acquired CfsVnode for the file is returned if it
     * is found.
     *
     * @param fileId the file id of the file
     * @return a boxed CfsVnode for the file if it exists, Empty if it does not exist,
     *         or Failure on error
     */
    def lookupById(fileId : CfsVFileId) : Box[CfsVnode] = {
        if (fileId.resource.id == 0L) {
            val err = Failure("lookupById called with 0 file id")
            Log.error(err.msg)
            err
        }
        else {
            DbManagerActor !! LookupByIdRequest(fileId) match {
                case Full(member: CfsVnode) ⇒
                    assert(member.isReferenced_?)
                    Full(member)
                case Full(e: EmptyBox) ⇒ e
                case _ ⇒ Failure("unexpected result from LookupByIdRequest")
            }
        }
    }

//    /**
//     * Determine if a file is 'special', meaning it has an associated MimeTypeHandler.
//     *
//     * @param vnode the file CfsVnode
//     * @return true if a MimeTypeHandler is associated with the file
//     */
//    def isSpecial_?(vnode : CfsVnode) : Boolean = {
//        DbManagerActor !! GetMimeTypeHandler(vnode) match {
//            case Full(_ : MimeTypeHandler) ⇒ true
//            case Full(e : EmptyBox) ⇒ false
//            case _ ⇒
//                Log.error(s"unexpect result from GetMimeTypeHandler for resource id ${vnode.getResourceId}")
//                false
//        }
//    }

//    /**
//     * Get a count of the number of members in a container.
//     *
//     * @param container the container CfsVnode
//     * @return a boxed count of members, or a Failure
//     */
//    def memberCount(container : CfsVnode) : Box[Long] = {
//        DbManagerActor !! MemberCountRequest(container) match {
//            case Full(count : Long) ⇒ Full(count)
//            case Full(e : EmptyBox) ⇒ e
//            case _ ⇒ Failure("unexpected result from MemberCountRequest")
//        }
//    }

    /**
     * Look for a given member Vnode in a container. If it is a member, return
     * its name in the container.
     *
     * @param container the container
     * @param member the possible member of the container
     * @return the boxed name of the member if it is a member of the container, Empty
     *         if it is not, or a Failure on error
     */
    def getMemberName(container : CfsVnode, member : CfsVnode) : Box[String] = {
        assert(container.isReferenced_?)
        getMemberName(container.getResourceId, member.getResourceId)
    }

    def getMemberName(container : ResourceId, member : ResourceId) : Box[String] = {
        DbManagerActor !! MemberNameRequest(container, member) match {
            case Full(name : String) ⇒ Full(name)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from MemberNameRequest")
        }
    }

    /**
     * Get a list of names of members of a container. It is assumed that the caller
     * has acquired the container, and it remains acquired.
     *
     * @param container the container CfsVnode
     * @return a boxed list of member names, or a Failure if, for example, the container
     *         does not exist
     */
    def members(container : CfsVnode) : Box[List[String]] = {
        assert(container.isReferenced_?)
        DbManagerActor !! MembersRequest(container) match {
            case Full(MembersResult(list)) ⇒ Full(list)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from MembersRequest")
        }
    }

    /**
     * Combine members() with a lookup() for each member. Return a list of tuples containing
     * member names and member vnodes. The members returned can be restricted to certain
     * MIME types by specifying a list of MIME type ids. If the list is omitted or Nil,
     * all members are returned.
     *
     * @param container the container CfsVnode
     * @param mtlist an optional list of MIME type ids used to filter members
     * @return a boxed list of tuples for members, or Failure if, for example, the container
     *         does not exist
     */
    def lookupMembers(container : CfsVnode, mtlist : List[MimeTypeId] = Nil) : Box[List[(String, CfsVnode)]] = {
        assert(container.isReferenced_?)
        DbManagerActor !! LookupMembersRequest(container, mtlist) match {
            case Full(LookupMembersResult(list)) ⇒ Full(list)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from LookupMembersRequest")
        }
    }

//    /**
//     * Open a file, given an associated CfsVnode. This creates a VFile to reference
//     * the file. The type of open will determine what kinds of shared access to the
//     * file will be possible. Normally a file cannot be deleted while there is an
//     * outstanding VFile for it, though it may be marked for delete. Some file types
//     * may provide an instance of a subclass of VFile, which may support additional
//     * operations on the file.
//     *
//     * The provided CfsPath is incorporated into the VFile to record the path used to
//     * locate the file Vnode. The path is not stored in the Vnode itself, since the
//     * same Vnode can be accessed through multiple paths.
//     *
//     * The principal must have appropriate access rights for the open operation,
//     * and the required rights may depend on the options.
//     *
//     * @param vnode the CfsVnode for this file
//     * @param path the filename path used to locate the file
//     * @param principal the principal responsible for this open operation
//     * @param options options that may affect the open operation
//     * @return a boxed VFile (or subclass) for the file associated with the vnode. A Failure
//     *         may be returned for various errors, such as insufficient access rights, or
//     *         unsupported options.
//     */
//    def open(vnode : CfsVnode, path : CfsPath,
//             principal : Principal, options : Map[String, Any]) : Box[VFile] = {
//        DbManagerActor !! OpenRequest(vnode, path, principal, options) match {
//            case Full(vh : VFile) ⇒ Full(vh)
//            case Full(e : EmptyBox) ⇒ e
//            case _ ⇒ Failure("unexpected result from OpenRequest")
//        }
//    }

    /**
     * Find all the parent containers of a given CfsVnode.
     *
     * @param member the member CfsVnode
     * @return a boxed list of all the parent CfsVnodes of the member, along with the
     *         filename of the member in each parent container
     */
    def findContainers(member : CfsVnode) : Box[List[(CfsVnode, String)]] = {
        assert(member.isReferenced_?)
        DbManagerActor !! FindContainersRequest(member) match {
            case Full(FindContainersResult(list)) ⇒
                list foreach { pair ⇒
                    val (vnode, _) = pair
                    assert(vnode.isReferenced_?)
                }
                Full(list)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from FindContainersRequest")
        }
    }

    /**
     * Find a path to a Vnode that is accessible to a given principal.
     *
     * @param vnode the vnode for which a path is requested
     * @param principal the principal responsible for this request
     * @return a boxed CfsPath which references the given Vnode
     */
    def findPath(vnode : CfsVnode, principal : Principal) : Box[CfsAbsolutePath] = {
        assert(vnode.isReferenced_?)
        DbManagerActor !! FindPathRequest(vnode, principal, all = false) match {
            case Full(path : CfsAbsolutePath) ⇒ Full(path)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from GetPathRequest")
        }
    }

    /**
     * Find all the paths to a Vnode that are accessible to a given principal.
     *
     * @param vnode the vnode for which paths are requested
     * @param principal the principal responsible for this request
     * @return a list of CfsPaths which references the given Vnode
     */
    def findPaths(vnode : CfsVnode, principal : Principal) : Box[List[CfsAbsolutePath]] = {
        assert(vnode.isReferenced_?)
        DbManagerActor !! FindPathRequest(vnode, principal, all = true) match {
            case Full(FindPathResult(list)) ⇒ Full(list)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from GetPathRequest")
        }
    }

    /**
     * Link a resource into a container, giving it a specified name. The reference count
     * of the member Resource is assumed to have been incremented to account for the
     * link being created, typically by a previous call to reference(). The caller is
     * responsible for decrementing the count (via dereference()) if this operation fails.
     *
     * @param container the container CfsVnode
     * @param member the new member CfsVnode
     * @param name the name that the member will have in the container
     * @return a boxed value of true if successful, otherwise a Failure. This will succeed
     *         if the link already exists as specified,
     */
    def link(container : CfsVnode, member : CfsVnode, name : String) : Box[Boolean] = {
        assert(container.isReferenced_?)
        assert(member.isReferenced_?)
        assert(container.isWriteLocked_?)
        assert(member.isWriteLocked_?)
        DbManagerActor !! LinkRequest(container, member, name) match {
            case Full(b : Boolean) ⇒ Full(b)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒
                val cid = container.getResourceId
                val mid = member.getResourceId
                Failure(s"unexpected result from LinkRequest($cid, $mid, $name")
        }
    }

    /**
     * Unlink a member from a container. This removes the link from the container to
     * the member, and decrements the member's Resource reference count. If that would
     * leave the Resource reference count as zero, the member must not have any members
     * of its own, or any metadata. In it does, the unlink is not done, and a list of
     * member's members is returned. These must be recursively unlinked before an unlink
     * of the member can succeed.
     *
     * The caller should hold a write lock on both the container and the member Vnodes,
     * acquired in that order.
     *
     * @param container the container CfsVnode
     * @param member the member name
     * @param mvnode  the member CfsVnode
     * @return a boxed pair containing a boolean value and a list. If the list is empty,
     *         the boolean indicates the status of a successful completion of the unlink.
     *         If the boolean is true, it indicates that there are no more links to the
     *         member file, which means it may be deleted when the last open file handle
     *         for it is closed. If the list is not empty, the boolean is irrelevant, and
     *         the list contains the names of members of the member (including the metadata
     *         folder name, if the member has metadata). These must be recursively
     *         unlinked before the original unlink can succeed. Failure is returned
     *         if the member is not actually a member of the container.
     */
    def unlink(container : CfsVnode, member : String, mvnode : CfsVnode) : Box[(Boolean, List[String])] = {
        assert(container.isReferenced_?)
        assert(mvnode.isReferenced_?)
        assert(container.isWriteLocked_?)
        assert(mvnode.isWriteLocked_?)
        DbManagerActor !! UnlinkRequest(container, member, mvnode) match {
            case Full(UnlinkResponse(b, list)) ⇒ Full((b, list))
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from UnlinkRequest(${container.getResourceId}, $member)")
        }
    }

    /**
     * This is used to increment the reference count of the Resource associated with a specified
     * Vnode, typically in anticipation of doing a link() operation to create a new filename
     * reference to the Resource.
     *
     * The caller should hold the write lock of the specified Vnode.
     *
     * Note that this is not the reference count of the Vnode itself, which is only maintained
     * in memory, and is used to indicate when a Vnode is eligible to be removed from DbManager's
     * resource cache.
     *
     * Note that it is not necessary to use this operation before linking a Vnode returned by
     * createFile(). The Resource associated with that Vnode will already have its reference
     * count set to 1.
     *
     * @param vnode the Vnode containing the Resource
     * @return the boxed resulting reference count of the Resource, or Failure
     */
    def reference(vnode : CfsVnode) : Box[Long] = {
        assert(vnode.isReferenced_?)
        assert(vnode.isWriteLocked_?)
        DbManagerActor !! ReferenceRequest(vnode) match {
            case Full(refcount : Long) ⇒ Full(refcount)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from ReferenceRequest(${vnode.getResourceId})")
        }
    }

    /**
     * This is used to decrement the reference count of the Resource associated with a specified
     * Vnode, typically when a link() operation has failed. If the Resource reference count goes
     * to zero, the Resource will be removed when the Vnode is closed. The Vnode's reference count
     * is not decremented by this operation, which means it will not close the Vnode.
     *
     * The caller should hold the write lock of the specified Vnode.
     *
     * Note that this is not the reference count of the Vnode itself, which is only maintained
     * in memory, and is used to indicate when a Vnode is eligible to be removed from DbManager's
     * resource cache.
     *
     * @param vnode the Vnode containing the Resource
     * @return the boxed resulting reference count of the Resource, or Failure
     */
    def dereference(vnode : CfsVnode) : Box[Long] = {
        assert(vnode.isReferenced_?)
        assert(vnode.isWriteLocked_?)
        DbManagerActor !! DereferenceRequest(vnode) match {
            case Full(refcount : Long) ⇒ Full(refcount)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from DereferenceRequest(${vnode.getResourceId})")
        }
    }

//    /**
//     * Get an input stream for a given CfsVnode. If the resource has no data, an empty
//     * stream is returned.
//     *
//     * @param vnode the CfsVnode
//     * @return a boxed InputStream, or Failure
//     */
//    // TODO: this is provisional. There will need to be a way to track open input streams
//    // at some point, associating them with a process. When file locking is added, that
//    // also will need awareness of open streams. And an open stream really should
//    // prevent the resource from being deleted, even if all paths to it are removed.
//    def getInputStream(vnode : CfsVnode) : Box[InputStream] = {
//        DbManagerActor !! InputStreamRequest(vnode) match {
//            case Full(in : InputStream) ⇒ Full(in)
//            case Full(e : EmptyBox) ⇒ e
//            case _ ⇒ Failure(s"unexpected result from InputStreamRequest(${vnode.getResourceId})")
//        }
//    }

    /**
     * Create a new file CfsVnode. The file data may or may not have already been stored.
     * The returned CfsVnode is acquired, and its associated Resource has a reference
     * count of one. The returned CfsVnode should either be linked into a container, or
     * dereferenced and closed (either of which should be done under a write lock).
     *
     * @param owner the principal who will own the file
     * @param mimetype the MIME type string
     * @param options options that may affect the create, including:
     *                dataSeqnum - ChoiceData sequence number of existing file data, default
     *                             is for the file to be created empty
     *                ctime - creation timestamp in milliseconds, default now
     *                altid - reference to some other table entry, default none
     * @return a boxed Vnode for the new file, or Failure
     */
    def createFile(owner : Principal, mimetype : String, options : CfsCreateOptions) : Box[CfsVnode] = {
        DbManagerActor !! CreateFileRequest(owner, mimetype, options) match {
            case Full(vnode : CfsVnode) ⇒
                assert(vnode.isReferenced_?)
                Full(vnode)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from CreateFileRequest(${owner.getPrincipalName}}, '$mimetype', ..)")
        }
    }

    def replaceData(vnode : CfsVnode, dataSeqnum : Long, mtime : Long) : Box[Vnode] = {
        assert(vnode.isReferenced_?)
        assert(vnode.isWriteLocked_?)
        DbManagerActor !! ReplaceDataRequest(vnode, dataSeqnum, mtime) match {
            case Full(nvnode : Vnode) ⇒ Full(nvnode)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from ReplaceDataRequest(${vnode.getResourceId}, $dataSeqnum, $mtime)")
        }
    }

    /**
     * Ensure a unique ChoiceData file for a specified Vnode. If the Vnode has no associated
     * ChoiceData file, an empty one is created. If it shares a ChoiceData file with another
     * Vnode, a private copy is made for this Vnode. In any case, this Vnode is effectively
     * removed from the data deduplication mechanism supported by Vnodes, either by unlinking
     * it from a shared DataNode, or removing any private DataNode of this Vnode.
     *
     * The purpose of this is to prepare the specified Vnode to produce an output stream
     * for its ChoiceData file.
     *
     * @param vnode the Vnode
     * @return the boxed sequence number of the private ChoiceData file
     */
    def makeUniqueData(vnode : CfsVnode) : Box[Long] = {
        assert(vnode.isReferenced_?)
        assert(vnode.isWriteLocked_?)
        DbManagerActor !! MakeUniqueDataRequest(vnode) match {
            case Full(seqnum : Long) ⇒ Full(seqnum)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from MakeUniqueDataRequest(${vnode.getResourceId})")
        }
    }

    /**
     * Set a limit on the number of VNodes cached in the lruResource cache.
     *
     * @param limit the maximum number of entries
     * @return the boxed current number of entries
     */
    def setVNodeCacheLimit(limit : Int) : Box[Int] = {
        if (limit < 1) Failure(s"invalid VNode cache limit $limit: must be at least 1")
        else DbManagerActor !! SetVNodeCacheLimitRequest(limit) match {
            case Full(count : Int) ⇒ Full(count)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from SetVNodeCacheLimitRequest($limit)")
        }
    }

    def clearVNodeCache() : Box[Int] = {
        DbManagerActor !! ClearVNodeCacheRequest match {
            case Full(limit : Int) ⇒ Full(limit)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from ClearVNodeCacheRequest")
        }
    }

    /**
     * Set a limit on the number of FsName entries cached in the lruFsName cache.
     *
     * @param limit the maximum number of entries
     * @return the boxed current number of entries
     */
    def setFsNameCacheLimit(limit : Int) : Box[Int] = {
        if (limit < 1) Failure(s"invalid FsName cache limit $limit: must be at least 1")
        else DbManagerActor !! SetFsNameCacheLimitRequest(limit) match {
            case Full(count : Int) ⇒ Full(count)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure(s"unexpected result from SetFsNameCacheLimitRequest($limit)")
        }
    }

    def clearFsNameCache() : Box[Int] = {
        DbManagerActor !! ClearFsNameCacheRequest match {
            case Full(limit : Int) ⇒ Full(limit)
            case Full(e : EmptyBox) ⇒ e
            case _ ⇒ Failure("unexpected result from ClearFsNameCacheRequest")
        }
    }

    //    /**
//     * Get ResourceInfo for a specified CfsVnode.
//     *
//     * @param vnode the CfsVnode
//     * @return a boxed ResourceInfo for the CfsVnode
//     */
//    def resourceInfo(vnode : CfsVnode) : Box[ResourceInfo] = {
//        DbManagerActor !! ResourceInfoRequest(vnode) match {
//            case Full(info : ResourceInfo) ⇒ Full(info)
//            case Full(e : EmptyBox) ⇒ e
//            case _ ⇒ Failure("unexpected result from ResourceInfoRequest")
//        }
//    }
//
    def registerMimeTypeHandler(mth : MimeTypeHandler) : DbManagerPlugin = {
        DbManagerActor ! RegisterMimeTypeHandler(mth, None)
        DbManagerInternal
    }

    def registerMimeTypeHandler(mth : MimeTypeHandler, mimeType : String) : DbManagerPlugin = {
        DbManagerActor ! RegisterMimeTypeHandler (mth, Some(mimeType))
        DbManagerInternal
    }

    def getMimeTypeHandler(mtid : MimeTypeId) : Box[MimeTypeHandler] = {
        DbManagerInternal dbmGetMimeTypeHandler mtid
    }

    def getMimeTypeHandler(mimeType : String) : Box[MimeTypeHandler] = {
        getMimeTypeHandler (getMimeTypeId (mimeType))
    }

    def getMimeTypeId(mimeType : String) : MimeTypeId = {
        DbManagerInternal dbmGetMimeTypeId mimeType
    }

    def initialize() : Unit = {
        // Wait for an empty response, so that we know initialization is done
        DbManagerActor !! InitializeDbManager
    }

    def uncacheVnode(id : ResourceId) : Boolean = {
        DbManagerActor !! UncacheVnodeRequest(id) match {
            case Full(b : Boolean) ⇒ b
            case _ ⇒ false
        }
    }

    def reviewCache() : Unit = {
        DbManagerActor ! ReviewCache
    }

    def validateCache : Map[String, Any] = {
        DbManagerActor !! ValidateCacheRequest match {
            case Full(ValidateCacheResponse(map)) ⇒ map
            case _ ⇒ Map("msg" → "failed")
        }
    }
}

object DbManager extends DbManager {
    val Log = Logger("choice.actor.DbManager")
}

trait DbManagerPlugin {
    /**
     * Get the resource for a filesystem root with a specified name. The default
     * root is named "/", but this allows the possibility of roots with other names,
     * as long as the names contain a character that is not valid in a regular filename.
     *
     * @param name the root name
     * @return the CfsVnode for the root Folder, which will be created if necessary
     */
    def dbmGetRoot(name : String) : CfsFnode

    def dbmClose(vnode : CfsVnode) : Box[Boolean]

    def dbmLookup(container : CfsVnode, name : String) : Box[CfsVnode]

    def dbmLookupById(fileId : CfsVFileId) : Box[CfsVnode]

    def dbmLookupById(resource : ResourceId) : Box[CfsVnode]

    def dbmMemberCount(container : CfsVnode) : Box[Long]

    def dbmMembers(container : CfsVnode) : Box[List[String]]

    def dbmLookupMembers(container : CfsVnode, mtlist : List[MimeTypeId]) : Box[List[(String, CfsVnode)]]

    def dbmGetMemberName(container : ResourceId, member : ResourceId) : Box[String]

//    def dbmOpenRequest(vnode : CfsVnode, path : CfsPath,
//                       principal : Principal, options : Map[String, Any]) : Box[VFile]

    def dbmFindContainers(member : CfsVnode) : Box[List[(CfsVnode, String)]]

    def dbmFindPath(vnode : CfsVnode, principal : Principal, root : String) : Box[CfsAbsolutePath]

    def dbmFindPaths(vnode : CfsVnode, principal : Principal, root : String) : List[CfsAbsolutePath]

    /**
     * See if a given 'ancestor' CfsVnode is an ancestor of another specified CfsVnode.
     *
     * @param ancestor the potential ancestor resource
     * @param resource the resource having the potential ancestor
     * @return true if the specified ancestor is in fact an ancestor of 'resource'
     */
    def dbmFindAncestor(ancestor : CfsVnode, resource : CfsVnode) : Boolean

    def dbmLink(container : CfsVnode, member : CfsVnode, name : String) : Box[Boolean]

    /**
     * Unlink a resource from a container. This removes the link from the container to
     * the resource, decrements the container's reference count, decrements the
     * resource's reference count, and if zero, deletes the resource.
     *
     * @param container the container Vnode
     * @param member the member name
     * @param mvnode  the member Vnode
     * @return a boxed Boolean which is true if the resource was deleted, false if the
     *         link was removed but other links to the resource remain. Failure is returned
     *         if the resource was not a member of the container.
     */
    def dbmUnlink(container : CfsVnode, member : String, mvnode : CfsVnode) : Box[(Boolean, List[String])]

    def dbmReference(vnode : CfsVnode) : Box[Long]

    def dbmDereference(vnode : CfsVnode) : Box[Long]

    def dbmCreateFile(owner : Principal, mimetype : String, options : CfsCreateOptions) : Box[Vnode]

    def dbmReplaceData(vnode : CfsVnode, dataSeqnum : Long, mtime : Long) : Box[Vnode]

    def dbmMakeUniqueData(vnode : CfsVnode) : Box[Long]

    def dbmSetVNodeCacheLimit(limit : Int) : Box[Int]

    def dbmClearVNodeCache() : Box[Int]

    def dbmSetFsNameCacheLimit(limit : Int) : Box[Int]

    def dbmClearFsNameCache() : Box[Int]

    def dbmGetMimeTypeHandler(vnode : CfsVnode) : Box[MimeTypeHandler]

    def dbmGetMimeTypeHandler(mtid : MimeTypeId) : Box[MimeTypeHandler]

    def dbmUncacheVnode(id : ResourceId) : Boolean

    def forAllVnodes(f : CfsVnode ⇒ Unit) : Unit
}

//object CacheTracer {
//    var ps : PrintStream = null
//    var saveps : PrintStream = null
//
//    def open() : Unit = {
//        import net.liftweb.http.provider.servlet.HTTPServletContext
//        LiftRules.context match {
//            case httpctx : HTTPServletContext ⇒
//                val path = httpctx.ctx.getRealPath("/WEB-INF/cachetrace.txt")
//                ps = new PrintStream(path)
//                ps.println("Cache Trace")
//            case _ ⇒
//        }
//    }
//
//    def disable() : Unit = { saveps = ps; ps = null }
//    def enable() : Unit = { ps = saveps }
//
//    def trace(s : String) : Unit = {
//        if (ps != null) ps.println(s)
//    }
//
//    def dump(header : String, stuff : Map[String, Any]) : Unit = {
//        trace(header)
//        stuff foreach {
//            case (name, value) ⇒
//                trace(s"  $name = $value")
//        }
//    }
//
//    def close() : Unit = {
//        ps.flush()
//        ps.close()
//        ps = null
//    }
//}

private object DbManagerInternal extends DbManagerPlugin {
    private val Log = Logger("choice.actor.DbManagerActor")

    import choice.lib.ExtendedBox.extendBox

//    var resourceMissCausedNameLookup : Int = 0
//    var resourceMissInProgress : Int = 0

    class CfsVnodeCache extends LRUCacheBase[ResourceId, CfsVnode] {

//        private var recursion : Int = 0

//        /**
//         * Return the element with the specified unique id. If the element is not in the cache,
//         * it is fetched and cached.
//         *
//         * @param key unique key of element
//         *
//         * @return the boxed element
//         */
//        override def get(key: ResourceId): Box[CfsVnode] = {
//            CacheTracer.trace(s"resource get: ${key.id}")
//            if (recursion != 0) {
//                Log.error(s"lruResource recursion = $recursion")
//            }
//            recursion += 1
//            val realData = Resource findByKey key.id flatMap { res ⇒
//                mimetypeMap getOrElse (res.getMimeTypeId, CfsPlain) instantiate res
//            }
//            val result = super.get(key)
//            result match {
//                case Full(vnode) ⇒
//                    realData match {
//                        case Full(rvnode) ⇒
//                            if (rvnode.getResource != vnode.getResource) {
//                                Log.error(s"lruResource stale data for ${key.id}")
//                            }
//                        case e : EmptyBox ⇒
//                            Log.error(s"lruResource stale data for ${key.id}: resource is deleted")
//                    }
//                case e : EmptyBox ⇒
//                    if (realData.isDefined) {
//                        Log.error(s"lruResource totally inexplicable shit")
//                    }
//            }
//            recursion -= 1
//            result
//        }

        /** On a cache miss, return the element with the given unique id */
        def fetch(id : ResourceId) : Box[CfsVnode] = {
//            resourceMissInProgress += 1
            val result = Resource findByKey id.id flatMap { res ⇒
                mimetypeMap getOrElse (res.getMimeTypeId, CfsPlain) instantiate res
            }
            result match {
                case e : EmptyBox ⇒
                    Log.error(s"lruResource failed to fetch id ${id.id}", e ?~ "Empty")
                case _ ⇒
            }
//            resourceMissInProgress -= 1
            result
        }

        /** Called when the specified element is being evicted from the cache */
        override def evicted(resid : ResourceId, elem : CfsVnode) : Unit = {
            elem invalidate ()
        }

        /**
         * Check whether an entry can be evicted. Requires a zero reference count.
         *
         * @param key the entry key
         * @param value the entry value
         * @return true if the entry can be evicted
         */
        override def canEvict(key: ResourceId, value: CfsVnode): Boolean = {
            value.getReferenceCount == 0
        }
    }

    private val rootCache = new mutable.HashMap[String, CfsFnode]

    /**
     * Cache of FsName entries from the database.
     *
     * This is used to find an FsName entry given the resource id of a containing
     * folder and the name of a member. The FsName records the membership of the
     * named member in the folder, and also provides the resource id of the
     * member object.
     *
     * This also provides a function to find an FsName entry given the resource id
     * of the containing folder and the resource id of a potential member.
     */
    private object lruFsName extends DbTableCache[(ResourceId, String), FsName](FsName) {

//        private var recursion : Int = 0

        /**
         * Container map. This maps a key consisting of (folder, member) resource ids
         * to an FsName entry. The entries in this map should also be in the main map.
         */
        val cMap : mutable.HashMap[(ResourceId, ResourceId), FsName] = mutable.HashMap[(ResourceId, ResourceId), FsName]()

//        /**
//         * Return the element with the specified unique id. If the element is not in the cache,
//         * it is fetched and cached.
//         *
//         * @param key unique key of element
//         *
//         * @return the boxed element
//         */
//        override def get(key: (ResourceId, String)): Box[FsName] = {
//            CacheTracer.trace(s"name get: ${key._1.id}/${key._2}")
//            if (recursion != 0) {
//                Log.error(s"lruFsName recursion = $recursion")
//            }
//            recursion += 1
//            if (resourceMissInProgress != 0) {
//                resourceMissCausedNameLookup += 1
//                Log.error(s"lruFsName get while lruResource miss in progress")
//            }
//            val realData = FsName find (By(FsName.folder, key._1.id), By(FsName.name, key._2))
//            val result = super.get(key)
//            result match {
//                case Full(fsn) ⇒ realData match {
//                    case Full(rfsn) ⇒
//                        if (fsn != rfsn) {
//                            Log.error(s"lruFsName stale data for (${key._1.id}, ${key._2}")
//                        }
//                    case e : EmptyBox ⇒
//                        Log.error(s"lruFsName stale data for (${key._1.id}, ${key._2}: item is deleted")
//                }
//                case e : EmptyBox ⇒
//                    if (realData.isDefined) {
//                        Log.error(s"lruFsName totally inexplicable shit")
//                    }
//            }
//            recursion -= 1
//            result
//        }

        /**
         * Cache an element if it's not already cached.
         *
         * @param elem the element to be cached
         */
        override def cache(elem: FsName) : Unit = {
            val key = (elem.getFolderId, elem.name.get)
            cache(key, elem)
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
        override def fetch(key: (ResourceId, String)) : Box[FsName] = {
            FsName findNameInContainer (key._2, key._1)
        }

        /** Cache the element that has the specified id */
        override def cache(key: (ResourceId, String), value: FsName): Unit = {
            super.cache(key, value)
            cMap update ((value.getFolderId, key._1), value)
        }

        /**
         * Evict the element that has specified id, if it is in the cache. This does not
         * check whether canEvict() is true, and does not call evicted().
         *
         * @param key unique key of element
         *
         * @return true if the element was in the cache
         */
        override def evict(key: (ResourceId, String)) : Boolean = {
            val result = lruMap get key match {
                case None ⇒ false
                case Some(entry) ⇒
                    val fsn = entry.value
                    cMap remove ((fsn.getFolderId, fsn.getResourceId))
                    super.evict(key)
            }
            result
        }

        def evict(fsn : FsName) : Boolean = {
            val key = (fsn.getFolderId, fsn.name.get)
            evict(key)
        }

        /**
         * Find an FsName entry for a given name and container. The entry is cached if it
         * is found.
         *
         * @param container the container resource id
         * @param name the name of a potential member of the container
         * @return a boxed FsName if found, otherwise Empty or Failure
         */
        def findNameInContainer(container : ResourceId, name : String) : Box[FsName] = {
            val key = (container, name)
            get(key) use { fsn ⇒
                if ((fsn.getFolderId != container) || (fsn.name.get != name)) {
                    Log.error("lruFsName findNameInContainer fault")
                }
            }
        }

        /**
         * Find an FsName entry for a given container and member resource id. The entry is
         * cached if it is found.
         *
         * @param container the container resource id
         * @param member the potential member resource id
         * @return a boxed FsName if found, otherwise Empty or Failure
         */
        def findResourceInContainer(container : ResourceId, member : ResourceId) : Box[FsName] = {
            cMap get ((container, member)) match {
                case Some(fsn) ⇒
                    if ((fsn.getFolderId != container) || (fsn.getResourceId != member)) {
                        Log.error("lruFsName findResourceInContainer fault")
                    }
                    Full(fsn)
                case None ⇒
                    // Query the database
                    FsName findResourceInContainer (member, container) use (this cache _)
            }
        }
    }

    /**
     * For container resources, maintain maps of known children. These maps are not
     * comprehensive. They contain only children that have been recently accessed.
     */
//    private val childCache = new ChildCache

    private val lruResource = new CfsVnodeCache

    /**
     * Get the resource for a filesystem root with a specified name. The default
     * root is named "/", but this allows the possibility of roots with other names,
     * as long as the names contain a character that is not valid in a regular filename.
     *
     * @param name the root name
     * @return the resource for the root Folder, which will be created if necessary
     */
    def dbmGetRoot(name : String) : CfsFnode = {
        val result = rootCache getOrElseUpdate (name, {
            FsName find By(FsName.name, name) flatMap { fsn ⇒
                lruResource get fsn.getResourceId match {
                    case Full(folder : CfsFnode) ⇒
                        Full(folder)
                    case Full(_) ⇒ Failure(s"root '$name' is not a folder")
                    case e : EmptyBox ⇒ e
                }
            } openOr {
                // Create an empty Resource for the root to capture its metadata
                Resource.storeResource(1L, CfsFolder.getMimeTypeId, 0L, millis, 0L) match {
                    case Full(res) ⇒
                        FsName addName (res, "/", res) match {
                            case Full(fsnroot) ⇒
                                val vnodebox = mimetypeMap getOrElse (res.getMimeTypeId, CfsPlain) instantiate res match {
                                    case Full(folder : CfsFnode) ⇒
                                        Full(folder)
                                    case Full(_) ⇒ Failure(s"root '$name' is not a folder")
                                    case e : EmptyBox ⇒ e
                                }
                                vnodebox foreach { vnode ⇒
                                    lruFsName cache fsnroot
                                    lruResource cache (res.getSafeKey, vnode)
                                }
                                vnodebox openOr sys.error(vnodebox.toString)
                            case e : EmptyBox ⇒
                                res remove ()
                                val errmsg = s"failed to create filesystem root $name"
                                Log.error(errmsg, e)
                                sys.error(errmsg)
                        }
                    case e : EmptyBox ⇒
                        Log.error("failed to create filesystem root " + name, e)
                        sys.error("failed to create filesystem root " + name)
                }
            }
        })
        result.acquire
        result
    }

    def dbmClose(vnode : CfsVnode) : Box[Boolean] = {
        vnode.getReferenceCount match {
            case n if n > 0 ⇒
                val fileRefs = vnode.getResource.getRefCount
                if (fileRefs <= 0L) {
                    Log.error(s"resource id ${vnode.getResourceId.id} is being closed with $n references, but $fileRefs file references")
                }
                Full(false)
            case n if n < 0 ⇒
                sys.error(s"dbmClose: vnode ${vnode.getResourceId} reference count is $n")
            case _ ⇒
                val resource = vnode.getResource
                // Are there any links to this file left in the filesystem?
                if (resource.getRefCount == 0) {
                    // If this Vnode has an associated MimeTypeHandler, is it willing to
                    // go along with the delete?
                    val okToDelete = mimetypeMap get resource.getMimeTypeId match {
                        case Some(mth) ⇒ mth delete vnode
                        case None ⇒ Full(true)
                    }
                    okToDelete flatMap { ok ⇒
                        if (ok) {
                            val resid = resource.getSafeKey
                            // Delete any attribute value assignments for this resource
                            AttrVal deleteAll resid
                            // Ensure it is not in the URL cache
                            CacheFilter remove resid
                            // Remove the Vnode from DbManager's resource cache
                            lruResource evict resid
//                            childCache removeResource resid
                            // Remove the Resource from the database, and possibly its DataNode
                            Full(resource remove ())
                        }
                        else Full(false)
                    }
                }
                else {
                    // If this Vnode has an associated ChoiceData file, but no DataNode, create
                    // a DataNode for it.
                    val seqnum = resource.getSeqnum
                    if ((seqnum >= 0) && (resource.dnode.get <= 0L)) {
                        resource replaceData (seqnum, millis)
                    }
                    Full(false)
                }
        }
    }

    def dbmLookup(container : CfsVnode, name : String) : Box[CfsVnode] = {
        val containerId = container.getResourceId
//        CacheTracer trace s"dbmLookup ${containerId.id}/$name"
        lruFsName findNameInContainer (containerId, name) flatMap { fsn ⇒
            lruResource get fsn.getResourceId use { member ⇒
                assert(member.getReferenceCount >= 0)
                member.acquire
                container.release
                assert(container.getReferenceCount >= 0)
            }
        }
//        childCache lookup (container.getResourceId, name) use { member ⇒
//            assert(member.getReferenceCount >= 0)
//            member.acquire
//            container.release
//            assert(container.getReferenceCount >= 0)
//        }
    }

    def dbmLookupById(fileId : CfsVFileId) : Box[CfsVnode] = dbmLookupById(fileId.resource)

    def dbmLookupById(resource : ResourceId) : Box[CfsVnode] = {
//        CacheTracer trace s"dbmLookupById ${resource.id}}"
        lruResource get resource use (_.acquire)
    }

    def dbmMemberCount(container : CfsVnode) : Box[Long] = {
        val result = (FsName containerLinks container.getResourceId foldLeft 0L) { (count, fsn) ⇒
            lruFsName cache fsn
            // Don't count container self-references (happens for filesystem root at least)
            if (fsn.getFolderId != fsn.getResourceId) count + 1 else count
        }
        Full(result)
    }

    def dbmMembers(container : CfsVnode) : Box[List[String]] = {
        Full(FsName containerLinks container.getResourceId collect {
            case fsn if fsn.getFolderId != fsn.getResourceId ⇒
                lruFsName cache fsn
                fsn.name.get
        })
    }

    def dbmLookupMembers(container : CfsVnode, mtlist : List[MimeTypeId]) : Box[List[(String, CfsVnode)]] = {

        val mtclause =
            if (mtlist == Nil) ""
            else {
                val mtliststring = mtlist.map(_.id).mkString("(", ",", ")")
                s""" and r.mtid in $mtliststring"""
            }
        val pstmt =
            s"""select f.name, r.* from fsname f, resource_t r where f.folder = ?
                | and f.resource_c != f.folder and r.id = f.resource_c$mtclause""".stripMargin

        val pairs = DB.use(DefaultConnectionIdentifier) { conn ⇒
            DB.prepareStatement(pstmt, conn) { ps ⇒
                ps.setLong(1, container.getResourceId.id)
                DB.exec(ps) { rs ⇒
                    val lb = new ListBuffer[(String, Resource)]()
                    val mi = Resource.buildMapper(rs)
                    while (rs.next()) {
                        val name = rs.getString(1)
                        val resource = Resource.createInstance(DefaultConnectionIdentifier, rs, mi)
                        val pair = (name, resource)
                        lb += pair
                    }
                    lb.toList
                }
            }
        }
        val result =
            for {
                (name, resource) ← pairs
                member ← (lruResource rawget resource.getSafeKey) orElse {
                    val mth = mimetypeMap getOrElse (resource.getMimeTypeId, CfsPlain)
                    ((mth instantiate resource) use { m ⇒ lruResource cache (m.getResourceId, m) }).toOption
                }
            } yield {
                assert(member.getReferenceCount >= 0)
                member.acquire
                (name, member)
            }
        Full(result)
    }

    def dbmGetMemberName(container : ResourceId, member : ResourceId) : Box[String] = {
        lruFsName findResourceInContainer (container, member) map (_.name.get)
    }

//    def dbmOpenRequest(vnode : CfsVnode, path : CfsPath,
//                       principal : Principal, options : Map[String, Any]) : Box[VFile] = {
//        vnode match {
//            case spnode : CfsSpecialNode ⇒
//                dbmGetMimeTypeHandler(spnode) match {
//                    case Full(mth) ⇒ mth open (vnode, path, principal, options)
//                    case e : EmptyBox ⇒ Failure(s"missing MimeTypeHandler for resource ${vnode.getResourceId}")
//                }
//            case plnode : CfsPlainNode ⇒ Full(new CfsPlain (path, principal, plnode))
//        }
//    }
//
    def dbmFindContainers(member : CfsVnode) : Box[List[(CfsVnode, String)]] = {
        val list = FsName references member.getResourceId filter { fsn ⇒
            fsn.getFolderId != fsn.getResourceId
        } flatMap { fsn ⇒
            lruFsName cache fsn
//            CacheTracer trace s"dbmFindContainers ${member.getResourceId.id}"
            val result = lruResource get fsn.getFolderId map { vnode ⇒
                vnode.acquire
                (vnode, fsn.name.get)
            }
            result.toList
        }
        Full(list)
    }

    def dbmFindPath(vnode : CfsVnode, principal : Principal, root : String = "/") : Box[CfsAbsolutePath] = {
//        CacheTracer trace s"dbmFindPath ${vnode.getResourceId.id}"
        // We will be searching from a resource up to the root, so establish
        // the identity of the root.
        val rootRes = dbmGetRoot(root)
        val rootId = rootRes.getResourceId
        def isRoot(vnode : CfsVnode) : Boolean = rootId == vnode.getResourceId
        // Helper to verify that a parent resource is accessible to the principal
        def checkParent(pvnode : CfsVnode) : Boolean = {
            // TODO: check that 'principal' can see and traverse 'pres'
            true
        }
        // Iterate recursively over parent tuples of a Vnode with a given partial path
        @tailrec
        def helper(parents : List[(CfsVnode, String)], path : List[String]) : List[String] = {
            parents match {
                case Nil ⇒ Nil
                case head :: tail ⇒
                    val (pvnode, name) = head
                    val bpath = if (checkParent(pvnode)) buildPath(pvnode, name :: path) else Nil
                    pvnode.release
                    bpath match {
                        case Nil ⇒ helper(tail, path)
                        case list ⇒
                            // Found a path. Release the vnodes of any remaining parents.
                            tail foreach (_._1.release)
                            list
                    }
            }
        }
        // Build a path to a resource for each of its parents
        def buildPath(vnode : CfsVnode, path : List[String]) : List[String] = {
            // If we've reached the root, the path is complete. Otherwise get the parents
            // of this resource and see if any lead to the root, depth-first.
            if (isRoot(vnode)) path
            else helper(dbmFindContainers(vnode) openOr Nil, path)
        }
        // TODO: check that 'principal' can see the resource
        val result = CfsPath(buildPath(vnode, Nil), CfsRootRoot)
        rootRes.release
        result
    }

    def dbmFindPaths(vnode : CfsVnode, principal : Principal, root : String = "/") : List[CfsAbsolutePath] = {
//        CacheTracer trace s"dbmFindPaths ${vnode.getResourceId.id}"
        val rootRes = dbmGetRoot (root)
        val rootId = rootRes.getResourceId
        def isRoot(vnode : CfsVnode) : Boolean = rootId == vnode.getResourceId
        // Helper to verify that a parent resource is accessible to the principal
        def checkParent(pvnode : CfsVnode) : Boolean = {
            // TODO: check that 'principal' can see and traverse 'pres'
            true
        }
        // Build a path to a resource for each of its parents
        def buildPath(vnode : CfsVnode, path : List[String], acc : List[List[String]]) : List[List[String]] = {
            if (isRoot(vnode)) path :: acc
            else dbmFindContainers(vnode) map { list ⇒
                val ptuples = list filter { pair ⇒
                    val (pvnode, _) = pair
                    checkParent(pvnode)
                }
                // Recursive depth-first expansion toward the root. The acc list
                // accumulates any paths that reach the root.
                val result = ptuples.foldLeft(acc) { (plist, pair) ⇒
                    val (pvnode, name) = pair
                    buildPath(pvnode, name :: path, plist)
                }
                list foreach { pair ⇒
                    val (pvnode, _) = pair
                    pvnode.release
                }
                result
            } openOr acc
        }
        // First ensure that the original resource exists and can be seen
        // TODO: check that 'principal' can see the vnode
        val result = buildPath(vnode, Nil, Nil) map { list ⇒ CfsAbsolutePath(CfsRootRoot, list) }
        rootRes.release
        result
    }

    /**
     * See if a given 'ancestor' CfsVnode is an ancestor of another specified CfsVnode.
     *
     * @param ancestor the potential ancestor CfsVnode
     * @param vnode the CfsVnode having the potential ancestor
     * @return true if the specified ancestor is in fact an ancestor of 'resource'
     */
    def dbmFindAncestor(ancestor : CfsVnode, vnode : CfsVnode) : Boolean = {
        val ancestorId = ancestor.getResourceId
        @tailrec
        def helper(list : List[ResourceId]) : Boolean = {
            list match {
                case Nil ⇒ false
                case _ ⇒
                    if (list contains ancestorId) true
                    else {
                        val prevgen = list flatMap { resid ⇒
                            FsName references resid filter { fsn ⇒
                                fsn.getFolderId != fsn.getResourceId
                            } map { fsn ⇒
                                lruFsName cache fsn
                                fsn.getFolderId
                            }
                        }
                        helper(prevgen)
                    }
            }
        }
        helper(List (vnode.getResourceId))
    }

    /**
     * Create a new filename link in a specified container to a given Vnode. The reference
     * count of the Resource associated with the member Vnode is assumed to have been
     * incremented to reflect the link being created here. The caller is responsible for
     * decrementing that count if this operation fails.
     *
     * Certain restrictions apply. The specified name must not already refer to an existing
     * file in the container. Also, the member must not be an ancestor container of the
     * specified container, to preserve the acyclic property of containment.
     *
     * @param container the container Vnode
     * @param member the Vnode to be added to the container
     * @param name the filename to be given to the Vnode in the container
     * @return a boxed value of true if successful, otherwise Failure
     */
    def dbmLink(container : CfsVnode, member : CfsVnode, name : String) : Box[Boolean] = {
        assert(member.getResource.getRefCount > 0)
        // See if the prospective member is an ancestor (closer to the root) of the container.
        // If so, we would be creating a cycle in the naming hierarchy, and that is not allowed.
        if (dbmFindAncestor (member, container)) Failure("cyclic link is rejected")
        else {
            lruFsName findNameInContainer (container.getResourceId, name) match {
                case Empty ⇒
                    FsName addName (container.getResource, name, member.getResource) flatMap { mfsn ⇒
                        lruFsName cache mfsn
                        Full(true)
                    }
                case Full(fsn) ⇒
                    val msg =
                        if (fsn.getResourceId == member.getResourceId) s"attempted link to self: $name"
                        else s"name conflict on link: $name"
                    Failure(msg)
                case f : Failure ⇒ f
            }
        }
    }

    /**
     * Unlink a member from a container. This removes the link from the container to
     * the member, and decrements the member's reference count. The member is not
     * immediately deleted if its reference count goes to zero, as there may still
     * be open file handles for it.
     *
     * @param container the container CfsVnode
     * @param member the member name
     * @param mvnode  the member Vnode
     * @return a boxed tuple containing a Boolean which is true if the member's
     *         reference count was decremented to zero, and false if not. If it is
     *         false, the second element of the tuple is a list of member names
     *         which must be unlinked before the container unlink can succeed.
     *         Failure is returned if the member was not a member of the container.
     */
    def dbmUnlink(container : CfsVnode, member : String, mvnode : CfsVnode) : Box[(Boolean, List[String])] = {
        val containerId = container.getResourceId
        val memberId = mvnode.getResourceId
        // First make sure that the container actually has a link to the member
        val result = (lruFsName findNameInContainer (containerId, member) flatMap { fsn ⇒
            val resource = mvnode.getResource
            // Helper function to actually do the unlink when all the necessary conditions
            // have been met.
            def unlinkHelper : Box[(Boolean, List[String])] = {
                lruFsName evict fsn
                if (mvnode.isContainer_? && resource.getRefCount > 1) {
                    // A container is being unlinked. Ordinarily, all the descendants of the
                    // container must be unlinked first, which means that any references to
                    // them in CacheFilter would have already been removed. However, if the
                    // container being removed is referenced through another name, its
                    // descendants will not be unlinked, since they will still be accessible
                    // through another path. Since it seems like a lot of work to track down
                    // all the resource ids of these descendants to remove them individually
                    // from CacheFilter, we just clear the entire cache.
                    //
                    // The common case where this situation occurs is when a container is
                    // renamed.
                    Log.info(s"Clearing front-end cache for $member")
                    CacheFilter clear ()
                }
                else {
                    CacheFilter remove resource.getSafeKey
                }
                fsn unlink resource match {
                    case Full(res) ⇒
                        Full((res.getRefCount <= 0, Nil))
                    case Empty ⇒ Full((true, Nil))
                    case f : Failure ⇒ f
                }
            }

            // If the resource reference count indicates other links to the member exist,
            // this link can be removed without concern for whether the member has members.
            if (resource.getRefCount > 1) unlinkHelper
            else {
                mvnode match {
                    case attrnode : AttrDefNode ⇒
                        if (AttrDef hasValues attrnode.getResourceId.id) {
                            Failure(s"attribute still has value assignments")
                        }
                        else unlinkHelper
                    case _ ⇒
                        // This is the last link to the member resource. If it has its own members,
                        // they must be unlinked before it can be unlinked.
                        dbmMembers (mvnode) match {
                            case Full(list) ⇒
                                if (list == Nil) unlinkHelper
                                else Full((false, list))
                            case Empty ⇒ unlinkHelper
                            case f : Failure ⇒ f
                        }
                }
            }
        }) ?~ s"resource id $memberId is not contained by resource id $containerId"
        result
    }

    def dbmReference(vnode : CfsVnode) : Box[Long] = vnode.getResource addReference ()

    def dbmDereference(vnode : CfsVnode) : Box[Long] = vnode.getResource removeReference ()

    def dbmCreateFile(owner : Principal, mimetype : String, options : CfsCreateOptions) : Box[CfsVnode] = {
        // Translate MIME type string to id
        val mtid = MimeType findOrCreate mimetype
        // Create a resource for the file with a reference count of one
        Resource storeResource(owner.getPrincipalId, mtid, options.dataSeqnum, options.ctime, options.altid) match {
            case Full(res) ⇒
                // Wrap the resource in the appropriate type of CfsVnode and cache it
                val vnodebox : Box[CfsVnode] = mimetypeMap getOrElse (mtid, CfsPlain) instantiate res
                vnodebox use { vnode ⇒
                    vnode.acquire
                    val resid = vnode.getResourceId
                    lruResource cache (resid, vnode)
                }
            case e : EmptyBox ⇒
                Failure(s"dbmCreateFile: error creating resource for '${owner.getPrincipalName}'", None, e)
        }
    }

    def dbmReplaceData(vnode : CfsVnode, dataSeqnum : Long, mtime : Long) : Box[Vnode] = {
        // TODO: This should be handled by the MimeTypeHandler
        vnode.getResource replaceData(dataSeqnum, mtime)
        Full(vnode)
    }

    def dbmMakeUniqueData(vnode : CfsVnode) : Box[Long] = {
        val resource = vnode.getResource
        CacheFilter remove resource.getSafeKey
        resource.getDataNode match {
            case Full(dnode) ⇒
                assert(resource.getSeqnum == dnode.seqnum.get)
                val result =
                    if (dnode.refcount.get > 1L) {
                        // Multiple files are using the ChoiceData file associated with this DataNode,
                        // so make an exclusive copy of the ChoiceData file.
                        val seqno = dnode.seqnum.get
                        val curpath = ChoiceData getPath seqno
                        val newseq = ChoiceData.getFileSeqNumber
                        val newpath = ChoiceData getPath newseq
                        Files.copy(curpath, newpath, StandardCopyOption.COPY_ATTRIBUTES)
                        dnode.unlink
                        newseq
                    }
                    else {
                        // Don't unlink the DataNode, because that would delete the ChoiceData file
                        // But delete the DataNode, since only this resource is using it
                        dnode.delete_!
                        resource.getSeqnum
                    }
                // This resource no longer references the DataNode, which may no longer exist.
                resource.dnode(Empty).seqnum(Full(result)).saveMe()
                Full(result)
            case _ : EmptyBox ⇒
                val oldseq = resource.getSeqnum
                if (oldseq > 0) {
                    // This could happen if the file was just opened for unique data, but it
                    // hasn't been closed yet.
                    Full(oldseq)
                }
                else {
                    val newseq = ChoiceData.getFileSeqNumber
                    val newpath = ChoiceData getPath newseq
                    Files.createFile(newpath)
                    resource setSeqnum newseq
                    Full(newseq)
                }
        }
    }

    def dbmSetVNodeCacheLimit(limit : Int) : Box[Int] = {
        Full(lruResource setLimit limit)
    }

    def dbmClearVNodeCache() : Box[Int] = {
        val result = lruResource.getLimit
        lruResource setLimit 0
        lruResource setLimit result
        Full(result)
    }

    def dbmSetFsNameCacheLimit(limit : Int) : Box[Int] = {
        Full(lruFsName setLimit limit)
    }

    def dbmClearFsNameCache() : Box[Int] = {
        val result = lruFsName.getLimit
        lruFsName setLimit 0
        lruFsName setLimit result
        Full(result)
    }

    def dbmGetMimeTypeHandler(vnode : CfsVnode) : Box[MimeTypeHandler] = {
        val mtid = vnode.getResource.getMimeTypeId
        Full(mimetypeMap getOrElse (mtid, CfsPlain))
    }

    def dbmGetMimeTypeHandler(mtid : MimeTypeId) : Box[MimeTypeHandler] = {
        Full(mimetypeMap getOrElse (mtid, CfsPlain))
    }

    def dbmGetMimeTypeId(mimeType : String) : MimeTypeId = {
        mimetypeIdMap get mimeType match {
            case Some(id) ⇒ id
            case None ⇒ MimeType findOrCreate mimeType
        }
    }

    override def dbmUncacheVnode(id : ResourceId) : Boolean = {
        CacheFilter remove id
        lruResource rawget id match {
            case Some(_) ⇒ lruResource.evict(id)
            case None ⇒ false
        }
    }

    def forAllVnodes(f : CfsVnode ⇒ Unit) : Unit = {
        lruResource foreach { pair ⇒
            val (_, vnode) = pair
            f(vnode)
        }
    }

    def dbmReviewCache() : Unit = {
//        CacheTracer.disable()
        Log.info("Root cache entries:")
        var zerocount = 0
        rootCache foreach { pair ⇒
            val (name, vnode) = pair
            val count = vnode.getReferenceCount
            if (count == 0) zerocount += 1
            else Log.info(s"    $name: resource id ${vnode.getResourceId.id}, ref count $count")
        }
        Log.info(s"    $zerocount entries with no references")
        Log.info("Resource cache entries:")
        zerocount = 0
        lruResource foreach { pair ⇒
            val (resid, vnode) = pair
            val count = vnode.getReferenceCount
            val path = dbmFindPath(vnode, BootPrincipal) map (_.toString) openOr "*no path*"
            if (count == 0) zerocount += 1
            else Log.info(s"    resource id $resid, ref count $count, path '$path'")
        }
        Log.info(s"    $zerocount entries with no references")
//        CacheTracer.enable()
    }

    def dbmValidateCache : Map[String, Any] = {
        def myResourceMap(vnode : CfsVnode) : Map[String, Any] = {
            val res = vnode.getResource
            Map("id" → res.id.get,
                "ownerId" → res.owner.get,
                "refcount" → res.refcount.get,
                "crtime" → res.crtime.get,
                "mtime" → res.mtime.get,
                "mtid" → res.mtid.get,
                "size" → res.size.get,
                "dnode" → res.dnode.get,
                "altid" → res.altid.get)
        }
        val rootmaps = rootCache map { pair ⇒
            val (name, vnode) = pair
            val count = vnode.getReferenceCount
            val (valid, msg) = validateVnode(vnode.getResourceId, vnode)
            Map("valid" → valid, "msg" → msg, "count" → count, "name" → name) ++ myResourceMap(vnode)
        }
        val resmaps = lruResource.MRUIterator map {
            case (resid, vnode) ⇒
                val count = vnode.getReferenceCount
                val (valid, msg) = validateVnode(resid, vnode)
                Map("valid" → valid, "msg" → msg, "count" → count) ++ myResourceMap(vnode)
        }
        val fsnmaps = lruFsName.MRUIterator map {
            case ((_, _), fsn) ⇒
                val (valid, msg) = validateFsName(fsn)
                Map[String, Any]("valid" → valid, "msg" → msg, "id" → fsn.getSafeKey.id, "folder" → fsn.folder.get,
                    "resource" → fsn.resource.get, "name" → fsn.name.get)
        }
//        CacheTracer.dump("vnodeStats", lruResource.getStatistics)
//        CacheTracer.dump("fsnameStats", lruFsName.getStatistics)
//        CacheTracer.close()
        Map("rootCache" → rootmaps, "vnodeCache" → resmaps.toList, "fsnameCache" → fsnmaps.toList,
           "vnodeStats" → lruResource.getStatistics, "fsnameStats" → lruFsName.getStatistics)
    }

    private def validateVnode(resid : ResourceId, vnode : CfsVnode) : (Boolean, Option[String]) = {
        val resource = vnode.getResource
        if (resource.getSafeKey != resid) {
            val msg = s"cache has Vnode ${vnode.getResourceId.id} under wrong key $resid"
            (false, Some(msg))
        }
        else Resource findByKey resid.id match {
            case Full(checkResource) ⇒
                if ((checkResource.mtime.get != resource.mtime.get) ||
                    (checkResource.crtime.get != resource.crtime.get) ||
                    (checkResource.mtid.get != resource.mtid.get) ||
                    (checkResource.size.get != resource.size.get) ||
                    (checkResource.dnode.get != resource.dnode.get) ||
                    (checkResource.owner.get != resource.owner.get) ||
                    (checkResource.refcount.get != resource.refcount.get) ||
                    (checkResource.altid.get != resource.altid.get)) {
                    val msg = s"cached resource differs from resource in database"
                    (false, Some(msg))
                }
                else {
                    val rescnt = resource.getRefCount
                    val actualrefs = FsName count By(FsName.resource, resource)
                    if (rescnt != actualrefs) {
                        val msg = s"bad reference count: $rescnt (actual $actualrefs)"
                        (false, Some(msg))
                    }
                    else {
                        val ressize = resource.size.get
                        if (ressize > 0) {
                            DataNode findByKey resource.dnode.get match {
                                case Full(dnode) ⇒
                                    if (ressize != dnode.size.get) {
                                        val msg = s"size $ressize does not match DataNode size ${dnode.size.get}"
                                        (false, Some(msg))
                                    }
                                    else {
                                        val seqnum = dnode.seqnum.get
                                        val path = ChoiceData.getPath(seqnum)
                                        if (Files.exists(path)) (true, None)
                                        else {
                                            val msg = s"ChoiceData file (seqnum $seqnum) does not exist"
                                            (false, Some(msg))
                                        }
                                    }
                                case Empty ⇒
                                    val msg = s"DataNode (id ${resource.dnode.get}) does not exist"
                                    (false, Some(msg))
                                case f : Failure ⇒
                                    val msg = s"error accessing DataNode (id ${resource.dnode.get}: ${f.msg}"
                                    (false, Some(msg))
                            }
                        }
                        else (true, None)
                    }
                }
            case Empty ⇒
                val msg = s"resource id ${resid.id} is in the cache but not the database"
                (false, Some(msg))
            case f : Failure ⇒
                val msg = s"error accessing resource id ${resid.id}: ${f.msg}"
                (false, Some(msg))
        }
    }

    private def validateFsName(fsn : FsName) : (Boolean, Option[String]) = {
        val fsnid = fsn.getSafeKey
        FsName findByKey fsnid match {
            case Full(checkFsName) ⇒
                if ((checkFsName.folder.get != fsn.folder.get) ||
                    (checkFsName.resource.get != fsn.resource.get) ||
                    (checkFsName.name.get != fsn.name.get)) {
                    val msg = s"cached FsName differs from FsName in database"
                    (false, Some(msg))
                }
                else Resource findByKey fsn.folder.get match {
                    case Full(_) ⇒ Resource findByKey fsn.resource.get match {
                        case Full(_) ⇒ (true, None)
                        case Empty ⇒
                            val msg = s"FsName resource id ${fsn.resource.get} is missing"
                            (false, Some(msg))
                        case f : Failure ⇒
                            val msg = s"error accessing FsName resource id ${fsn.resource.get}: ${f.msg}"
                            (false, Some(msg))
                    }
                    case Empty ⇒
                        val msg = s"FsName folder resource id ${fsn.folder.get} is missing"
                        (false, Some(msg))
                    case f : Failure ⇒
                        val msg = s"error accessing FsName folder resource id ${fsn.folder.get}: ${f.msg}"
                        (false, Some(msg))
                }
            case Empty ⇒
                val msg = s"FsName id ${fsnid.id} is in the cache but not the database"
                (false, Some(msg))
            case f : Failure ⇒
                val msg = s"error accessing FsName id ${fsnid.id}: ${f.msg}"
                (false, Some(msg))
        }
    }

    /** Map of MIME type id to MimeTypeHandler, built from MimeTypeHandler registrations */
    private val mimetypeMap = collection.mutable.Map[MimeTypeId, MimeTypeHandler]()

    private val mimetypeIdMap = collection.mutable.Map[String, MimeTypeId]()

    def registerHandler(handler : MimeTypeHandler, mimeType : Option[String]) : Unit = {
        val name = mimeType getOrElse handler.getMimeType
        val id = dbmGetMimeTypeId(name)
        if (mimetypeMap contains id) {
            Log.error(s"multiple registrations for MIME type: $name")
        }
        mimetypeMap put (id, handler)
        mimetypeIdMap put (name, id)
        Log.info(s"registered handler for MIME type: $name")
    }
}

/**
 * The DbManager is responsible for maintaining the filesystem structure in the database.
 * Generally, all filesystem metadata manipulation should be done using DbManager one of
 * two interfaces provided by DbManager. The DbManager interface makes requests to
 * DbManager through messages sent to its actor. This is used by the FileManager. The
 * DbManagerPlugin interface is used by MimeTypeHandlers, which may be invoked as result
 * of DbManagerActor requests. Because the actor interface serializes requests, a
 * MimeTypeHandler request to any higher-level code that uses the DbManagerActor
 * interface will result in a deadly embrace.
 *
 * User: Hep
 * Date: 6/18/13
 * Time: 4:18 PM
 */
object DbManagerActor extends LiftActor with ListenerManager {
    private val Log = Logger("choice.actor.DbManagerActor")

    import choice.actor.DbManagerInternal._

    /** Set to true when DbManager.initialize() has been called and processed */
    private var initReceived = false

    /** Saved RegisterMimeTypeHandler messages that are received prior to initReceived */
    private var pendingRegistrations : List[(MimeTypeHandler, Option[String])] = Nil

    /**
     * Message to get a root container. The default root is named "/". The result is a
     * DbResource for the root.
     *
     * @param name optional alternate root name
     */
    sealed case class GetRootRequest(name : String = "/") {
        Log.debug("GetRootRequest(" + name + ")")
    }

    /**
     * Message to process a Vnode which just had its reference count decremented to
     * zero. Note that the count may have been incremented by the time this message
     * is dispatched.
     *
     * @param vnode the Vnode in question
     */
    sealed case class CloseRequest(vnode : CfsVnode)

    /**
     * Message to lookup a name in a container. The result is a boxed DbResource,
     * which is Empty if the name does not exist in the container.
     *
     * @param container the resource id of the container
     * @param name the name to look up
     */
    sealed case class LookupRequest(container : CfsVnode, name : String) {
        Log.debug("LookupRequest(" + container + ", " + name + ")")
    }

    sealed case class LookupByIdRequest(fileId : CfsVFileId)

//    sealed case class MemberCountRequest(container : CfsVnode)

    sealed case class MemberNameRequest(container : ResourceId, member : ResourceId)

    sealed case class MembersRequest(container : CfsVnode) {
        Log.debug("MembersRequest(" + container + ")")
    }

    sealed case class MembersResult(list : List[String])

    sealed case class LookupMembersRequest(container : CfsVnode, mtlist : List[MimeTypeId])
    sealed case class LookupMembersResult(list : List[(String, CfsVnode)])

    sealed case class ReferenceRequest(vnode : CfsVnode)
    sealed case class DereferenceRequest(vnode : CfsVnode)

    sealed case class FindContainersRequest(member : CfsVnode)
    sealed case class FindContainersResult(list : List[(CfsVnode, String)])

    sealed case class FindPathRequest(vnode : CfsVnode, principal : Principal, all : Boolean)
    sealed case class FindPathResult(list : List[CfsAbsolutePath])

    sealed case class LinkRequest(container : CfsVnode, member : CfsVnode, name : String)
    sealed case class UnlinkRequest(container : CfsVnode, member : String, mvnode : CfsVnode)
    sealed case class UnlinkResponse(nolinks : Boolean, members : List[String])

//    sealed case class OpenRequest(vnode : CfsVnode, path : CfsPath, principal : Principal, options : Map[String, Any])

//    sealed case class InputStreamRequest(vnode : CfsVnode)

    /**
     * Request to create a CfsVnode with a given name and MIME type and owner.
     * The file data, if any, is referenced by a data sequence number obtained from
     * ChoiceData. Some types of files may reference entries in other database tables,
     * using altId.
     *
     * @param owner     the resource id of the file owner principal
     * @param mimetype	the MIME type of the file
     * @param options optional options that may affect the create
     */
    sealed case class CreateFileRequest(owner : Principal, mimetype : String, options : CfsCreateOptions) {
        Log.debug(s"CreateFileRequest($mimetype, $options")
    }

    sealed case class ReplaceDataRequest(vnode : CfsVnode, dataSeqnum : Long, mtime : Long)

    sealed case class MakeUniqueDataRequest(vnode : CfsVnode)

    sealed case class SetVNodeCacheLimitRequest(limit : Int)

    case object ClearVNodeCacheRequest

    sealed case class SetFsNameCacheLimitRequest(limit : Int)

    case object ClearFsNameCacheRequest

    sealed case class RegisterMimeTypeHandler(mth : MimeTypeHandler, mimeType : Option[String])

    sealed case class UncacheVnodeRequest(id : ResourceId)

//    sealed case class GetMimeTypeHandler(vnode : CfsVnode)

    case object InitializeDbManager

    case object ReviewCache

    case object ValidateCacheRequest
    case class ValidateCacheResponse(map : Map[String, Any])

    def createUpdate : Any = Empty

    override def mediumPriority : PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
        override def isDefinedAt(msg : Any) : Boolean = {
            true
        }
        override def apply(msg : Any) : Unit = {
            msg match {
                case GetRootRequest(name) ⇒ doGetRootRequest(name)
                case CloseRequest(vnode) ⇒ doCloseRequest(vnode)
                case LookupRequest(container, name) ⇒ doLookupRequest(container, name)
                case LookupByIdRequest(id) ⇒ doLookupByIdRequest(id)
//                case MemberCountRequest(container) ⇒ doMemberCountRequest(container)
                case MemberNameRequest(container, member) ⇒ doMemberNameRequest(container, member)
                case MembersRequest(container) ⇒ doMembersRequest(container)
                case LookupMembersRequest(container, mtlist) ⇒ doLookupMembersRequest(container, mtlist)
//                case OpenRequest(vnode, path, principal, options) ⇒
//                    doOpenRequest(vnode, path, principal, options)
                case FindContainersRequest(member) ⇒ doFindContainersRequest(member)
                case FindPathRequest(vnode, principal, all) ⇒
                    doFindPathRequest(vnode, principal, all)
                case LinkRequest(container, member, name) ⇒ doLinkRequest(container, member, name)
                case UnlinkRequest(container, member, mvnode) ⇒ doUnlinkRequest(container, member, mvnode)
                case ReferenceRequest(vnode) ⇒ doReferenceRequest(vnode)
                case DereferenceRequest(vnode) ⇒ doDereferenceRequest(vnode)
                case CreateFileRequest(owner, mimetype, options) ⇒
                    doCreateFileRequest(owner, mimetype, options)
                case ReplaceDataRequest(vnode, dataSeqnum, mtime) ⇒
                    doReplaceDataRequest(vnode, dataSeqnum, mtime)
                case MakeUniqueDataRequest(vnode) ⇒ doMakeUniqueDataRequest(vnode)
                case SetVNodeCacheLimitRequest(limit) ⇒ doSetVNodeCacheLimit(limit)
                case ClearVNodeCacheRequest ⇒ doClearVNodeCacheRequest()
                case SetFsNameCacheLimitRequest(limit) ⇒ doSetFsNameCacheLimit(limit)
                case ClearFsNameCacheRequest ⇒ doClearFsNameCacheRequest()
//                case GetMimeTypeHandler(vnode) ⇒ doGetMimeTypeHandler(vnode)
                case RegisterMimeTypeHandler(mth, mimeType) ⇒ doRegisterMimeTypeHandler(mth, mimeType)
                case UncacheVnodeRequest(id) ⇒ doUncacheVnodeRequest(id)
                case InitializeDbManager ⇒ doInitializeDbManager()
                case ValidateCacheRequest ⇒ doValidateCacheRequest()
                case ReviewCache ⇒ doReviewCache()
            }
        }
    }

    private def doGetRootRequest(name : String) : Unit = {
        val result = dbmGetRoot(name)
        reply(result)
    }

    private def doCloseRequest(vnode : CfsVnode) : Unit = {
        val result = dbmClose(vnode)
        reply(result)
    }

    private def doLookupRequest(container : CfsVnode, name : String) : Unit = {
        val result = dbmLookup(container, name)
        reply(result openOr result)
    }

    private def doLookupByIdRequest(fileId : CfsVFileId) : Unit = {
        val result = dbmLookupById(fileId)
        reply(result openOr result)
    }

//    private def doMemberCountRequest(container : CfsVnode) : Unit = {
//        val result = dbmMemberCount(container)
//        reply(result openOr result)
//    }

    private def doMemberNameRequest(container : ResourceId, member : ResourceId) : Unit = {
        val result = dbmGetMemberName(container, member)
        reply(result openOr result)
    }

    private def doMembersRequest(container : CfsVnode) : Unit = {
        val result = dbmMembers(container) map MembersResult.apply
        reply(result openOr result)
    }

    private def doLookupMembersRequest(container : CfsVnode, mtlist : List[MimeTypeId]) : Unit = {
        val result = dbmLookupMembers(container, mtlist) map LookupMembersResult.apply
        reply(result openOr result)
    }

//    private def doOpenRequest(vnode : CfsVnode, path : CfsPath, principal : Principal, options : Map[String, Any]) : Unit = {
//        val result = dbmOpenRequest(vnode, path, principal, options)
//        reply(result openOr result)
//    }

    private def doFindContainersRequest(member : CfsVnode) : Unit = {
//        CacheTracer trace s"doFindContainersRequest ${member.getResourceId.id}"
        val result = dbmFindContainers(member) map FindContainersResult.apply
        reply(result openOr result)
    }

    private def doFindPathRequest(vnode : CfsVnode, principal : Principal, all : Boolean) : Unit = {
        if (all) {
            val result = dbmFindPaths (vnode, principal)
            reply(FindPathResult(result))
        }
        else {
            val result = dbmFindPath (vnode, principal)
            reply(result openOr result)
        }
    }

    private def doLinkRequest(container : CfsVnode, member : CfsVnode, name : String) : Unit = {
        val result = dbmLink(container, member, name)
        reply(result openOr result)
    }

    private def doUnlinkRequest(container : CfsVnode, member : String, mvnode : CfsVnode) : Unit = {
        val result = dbmUnlink(container, member, mvnode) match {
            case Full((b, list)) ⇒ UnlinkResponse(b, list)
            case e : EmptyBox ⇒ e
        }
        reply(result)
    }

    private def doReferenceRequest(vnode : CfsVnode) : Unit = {
        val result = dbmReference(vnode)
        reply(result openOr result)
    }

    private def doDereferenceRequest(vnode : CfsVnode) : Unit = {
        val result = dbmDereference(vnode)
        reply(result openOr result)
    }

//    private def doInputStreamRequest(vnode : CfsVnode) : Unit = {
//        val result = dbmInputStream(vnode)
//        reply(result openOr result)
//    }

    private def doCreateFileRequest(owner : Principal, mimetype : String, options : CfsCreateOptions) : Unit = {
        val result = dbmCreateFile(owner, mimetype, options)
        reply(result openOr result)
    }

    private def doReplaceDataRequest(vnode : CfsVnode, dataSeqnum : Long, mtime : Long) : Unit = {
        val result = dbmReplaceData(vnode, dataSeqnum, mtime)
        reply(result openOr result)
    }

    private def doMakeUniqueDataRequest(vnode : CfsVnode) : Unit = {
        val result = dbmMakeUniqueData(vnode)
        reply(result openOr result)
    }

    private def doSetVNodeCacheLimit(limit : Int) : Unit = {
        val result = dbmSetVNodeCacheLimit(limit)
        reply(result openOr result)
    }

    private def doClearVNodeCacheRequest() : Unit = {
        val result = dbmClearVNodeCache()
        reply(result openOr result)
    }

    private def doSetFsNameCacheLimit(limit : Int) : Unit = {
        val result = dbmSetFsNameCacheLimit(limit)
        reply(result openOr result)
    }

    private def doClearFsNameCacheRequest() : Unit = {
        val result = dbmClearFsNameCache()
        reply(result openOr result)
    }

    //    private def doGetMimeTypeHandler(vnode : CfsVnode) : Unit = {
//        val result = dbmGetMimeTypeHandler(vnode)
//        reply(result openOr result)
//    }

    private def doRegisterMimeTypeHandler(mth : MimeTypeHandler, mimeType : Option[String]) : Unit = {
        if (initReceived) registerHandler (mth, mimeType)
        else pendingRegistrations ::= ((mth, mimeType))
    }

    private def doUncacheVnodeRequest(id : ResourceId) : Unit = {
        val result = dbmUncacheVnode(id)
        reply(result)
    }

    private def doValidateCacheRequest() : Unit = {
        reply(ValidateCacheResponse(dbmValidateCache))
    }

    private def doInitializeDbManager() : Unit = {
//        CacheTracer.open()
        pendingRegistrations foreach (pair ⇒ registerHandler(pair._1, pair._2))
        pendingRegistrations = Nil
        initReceived = true
        reply(Empty)
    }

    private def doReviewCache() : Unit = { dbmReviewCache() }
}
