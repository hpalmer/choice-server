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

import choice.model.{ResourceId, Resource}
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.locks.ReentrantReadWriteLock
import choice.access.{RightId, PolicyRef, Principal}
import net.liftweb.common.{EmptyBox, Full, Failure, Box}
import net.liftweb.util.Schedule
import choice.actor.DbManager
import choice.lib.ExtendedBox.extendBox
import choice.fs.vfs.{VPath, VFile, VDirNode, Vnode}
import choice.core.CacheFilter

/**
 * In Cfs, every file is potentially a container for associated metadata files.
 * However, all descendants of an ordinary file must be of type "choice/metadata".
 * Files which can contain members of other types are subclasses of CfsFolder,
 * and are generically known as 'folders'. Non-folders are designated as 'plain'.
 *
 * @param resource the DB Resource entry for this file
 */
abstract class CfsVnode(resource : Resource) extends VDirNode {
    import Cfs.Log

    private val refCount = new AtomicLong(0L)
    private val rwlock = new ReentrantReadWriteLock()

    /**
     * Perform an operation while holding a read lock on the Vnode.
     *
     * @param f a function returning a result
     * @tparam T the type of value returned by f
     * @return the return value of f
     */
    def withReadLock[T](f : ⇒ () ⇒ T) : T = {
        rwlock.readLock().lock()
        val result = f()
        rwlock.readLock().unlock()
        result
    }

    /**
     * Perform an operation while holding a write lock on the Vnode.
     * The write lock can optionally be downgraded to a read lock during
     * the operation, by calling writeToReadLock(). Whichever lock is held
     * at the end of the operation will be released.
     *
     * @param f a function returning a result
     * @tparam T the type of value returned by f
     * @return the return value of f
     */
    def withWriteLock[T](f : ⇒ () ⇒ T) : T = {
        rwlock.writeLock().lock()
        val result = f()
        if (rwlock.writeLock().isHeldByCurrentThread) rwlock.writeLock().unlock()
        else rwlock.readLock().unlock()
        result
    }

    /**
     * Downgrade write lock to read lock.
     */
    def writeToReadLock() : Unit = {
        rwlock.readLock().lock()
        rwlock.writeLock().unlock()
    }

    /**
     * Is this Vnode write locked by any thread?
     *
     * @return true if so
     */
    def isWriteLocked_? : Boolean = rwlock.isWriteLocked

    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * a CfsFile that references this CfsVnode. Using the CfsFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * This method is mainly for a subclass of CfsVnode to return a corresponding
     * subclass of CfsFile, though it may do other things. Common open() processing
     * should be done in CfsVnode open().
     *
     * The reference count of this CfsVnode is assumed to have already been
     * incremented prior to calling its cfsOpen() method. If cfsOpen() is
     * successful, the reference count will be decremented when the returned
     * CfsFile handle is closed. If cfsOpen() fails, the caller (usually
     * CfsVnode open()) is responsible for decrementing the reference count.
     *
     * @param path the path that led to this CfsVnode
     * @param principal the principal to be associated with the CfsFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting CfsFile
     * @return a boxed CfsFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    def cfsOpen(path : CfsAbsolutePath, principal : Principal, options : CfsOpenOptions) : Box[CfsFile]

    /**
     * Helper function for implementations of cfsOpen, which releases this Vnode
     * and returns a Failure.
     *
     * @param msg message text for the returned Failure
     * @return Failure
     */
    def cfsOpenFailure(msg : String) : Failure = {
        assert(this.isReferenced_?)
        this.release
        Failure(msg)
    }

    /**
     * Alternate version, which just releases this Vnode and returns a given EmptyBox.
     *
     * @param e an EmptyBox to be returned
     * @return e
     */
    def cfsOpenFailure(e : EmptyBox) : EmptyBox = {
        assert(this.isReferenced_?)
        this.release
        e
    }

    /**
     * Subclasses override this function in order to do custom file creation processing.
     * That may include processing create options which are specific to the file type.
     * The subclass also has the option to return a different Vnode than the current one.
     * If it does, the current Vnode should be released, and the returned Vnode should be
     * acquired.
     *
     * @param name the requested name of the member (which may be changed later via an
     *             overload of the acceptName_? method)
     * @param principal the principal requesting the file creation
     * @param options options for the file creation, possibly file type specific
     * @return a boxed CfsVnode (usually this one) if successful, otherwise Failure
     */
    def cfsCreate(name : String, principal : Principal, options : CfsCreateOptions) : Box[CfsVnode] = Full(this)

    /******************* Begin Vnode implementation *************************************/

    /**
     * Execute some operation supported by this file type. This provides extensibility
     * for different file types. An operation may act on the file, the VFile, or both.
     * An operation may change the how a subsequent getInputStream() or getOutputStream()
     * works.
     *
     * @param vhandle a file handle previously returned from open()
     * @param op the operation name, which should be recognized by the file type
     * @param options parameters and options for the operation. These depend on
     *                the operation.
     * @return a boxed map of results. If the operation fails, Empty or Failure.
     */
    override def execute(vhandle : VFile, op : String,
                         options : Map[String, Any]) : Box[Map[String, Any]] = {
        Failure("execute is not yet supported")
    }

    /**
     * Get the VFileId associated with this file. Each file within a particular
     * filesystem should have a unique VFileId.
     *
     * @return a unique id for this file within its filesystem
     */
    override def getFileId : CfsVFileId = withReadLock (() ⇒ CfsVFileId(resource.getSafeKey))

    /**
     * Retrieve information about the file referenced by this Vnode. Some
     * filesystems may support options affecting the information returned.
     *
     * @param options optional options for operation
     * @return a VInfo instance
     */
    override def info(options : Option[Map[String, Any]] = None) : CfsInfo = withReadLock { () ⇒
        val res = getResource
        CfsInfo(res.getMimeType, res.getCreation, res.getLastModified,
                   res.getSize, res.refcount.get, res.getAlternateId)
    }

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean

    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * some subclass of VFile that references this Vnode. Using the VFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * @param path the path that led to this Vnode
     * @param principal the principal to be associated with the VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    override def open(path : VPath, principal : Principal, options : CfsOpenOptions) : Box[CfsFile] = {
        assert(this.isReferenced_?)
        val filebox = path match {
            case abspath : CfsAbsolutePath ⇒ cfsOpen(abspath, principal, options)
            case relpath : CfsRelativePath ⇒
                Log.error(s"CfsVnode open called with relative path: ${relpath.toString}")
                cfsOpen(CfsAbsolutePath(CfsRootRoot, relpath.parts), principal, options)
            case rootpath : CfsRootPath ⇒
                cfsOpenFailure(s"CfsVnode open called with root path: ${rootpath.toString}")
            case vpath ⇒ cfsOpenFailure(s"non-Cfs path for CfsVnode: $path")
        }
        if (filebox.isEmpty) this.release
        filebox
    }

    /**
     * Change the owner of the file associated with this Vnode. This operation
     * should be restricted to the current owner of the file, or a system
     * administrator, but that is not done at this level.
     *
     * @param principal the principal who will be the new owner of the file
     * @return the boxed Vnode, with the updated owner
     */
    def chown(principal : Principal) : Box[CfsVnode] = withWriteLock { () ⇒
        getResource.setOwner(principal.getPrincipalId) map (_ ⇒ this)
    }

    /**
     * Acquire a reference to this Vnode. This is normally done automatically
     * on a Vnode returned from a lookup() operation. And the open() operation
     * assumes that the caller has acquired a reference to the Vnode. If open()
     * succeeds then releasing the reference becomes the responsibility of the
     * returned VFile. If open() fails, the reference is automatically released.
     *
     * @return the current number of dynamic references to the Vnode
     */
    def acquire : Long = refCount incrementAndGet ()

    /**
     * Release a previously acquired reference to this Vnode. The remaining
     * number of references is returned. The Vnode becomes eligible for
     * deallocation when the count goes to zero, but is not necessarily
     * deallocated immediately. An exception may be thrown if release()
     * is called when the count is zero.
     *
     * @return the number of references to this Vnode remaining
     */
    override def release : Long = refCount.decrementAndGet match {
        case n if n < 0 ⇒ sys.error("reference count decremented past zero")
        case n if n == 0 ⇒
            // Since DbManager calls release, this needs to be done on a
            // different thread.
            Schedule (() ⇒ DbManager close this)
            n
        case n ⇒ n
    }

    override def isReferenced_? : Boolean = refCount.get() > 0

    def getReferenceCount : Long = refCount.get()

    /**************** End Vnode implementation **************************************/

    /**************** Begin VDirNode implementation *********************************/

    /**
     * This returns a boolean indicating whether the container represented by
     * this VDirNode can contain members of a specified MIME type. Only members
     * of an acceptable MIME type should be created or linked in the container.
     *
     * @param mimetype the MIME type of a proposed new member
     * @return true if the container accepts the MIME type, otherwise false
     */
    def acceptType_?(mimetype : String) : Boolean = {
        import CfsVnode.{parentRestrictions, childRestrictions}
        val containerType = this.getMimeType
        // TODO: The theory was that any file could serve as a container for "choice/metadata"
        // members, but this has not been implemented, and may never be.
        mimetype == "choice/metadata" || {
            // The member must accept the parent container
            (parentRestrictions get mimetype fold true)(_ contains containerType) &&
                // And the parent container must accept the member
                (childRestrictions get containerType fold true)(_ contains mimetype)
        }
    }

    /**
     * Check the validity of a proposed member name for this container. The
     * container need not be concerned with whether it already has a member of
     * the same name, as that will be checked when the member is linked to the
     * container. But some containers may impose specific constraints on the
     * syntax of member names, and some containers may care about name conflicts
     * that go beyond immediate members.
     *
     * This also checks whether the MIME type of the proposed member is acceptable
     * to the container, via a call to acceptType_?().
     *
     * The boxed name to be used for the member is returned. It may have been
     * adjusted from the originally proposed name.
     *
     * @param name the proposed name of a new member
     * @param member the CfsVnode of the member
     * @return the boxed name to use for the member, or Failure
     */
    def acceptName_?(name : String, member : CfsVnode) : Box[String] = {
        val memberType = member.getMimeType
        if (acceptType_?(memberType)) Full(name)
        else {
            val containerType = this.getMimeType
            Failure(s"container type $containerType does not accept member '$name' of type $memberType")
        }
    }

    /**
     * Link a given Vnode as a member of this container with a specified name.
     * The name may be modified by some containers, for example, to enforce special
     * syntax constraints. The returned name is the actual name of the member.
     *
     * It is assumed that the reference count of the Resource associated with the
     * incoming Vnode has already been incremented to account for the link being
     * created. If this call fails, that count should be decremented by the caller
     * before releasing the Vnode.
     *
     * @param name the filename given to the Vnode in this container
     * @param member the Vnode to be linked as a member
     * @return the boxed name of member if successful, or Failure on error.
     */
    override def link(name : String, member : Vnode) : Box[String] = withWriteLock { () ⇒
        member match {
            case cfsmem : CfsVnode ⇒ cfsmem.withWriteLock { () ⇒
                this acceptName_? (name, cfsmem) flatMap { memberName ⇒
                    DbManager link (this, cfsmem, memberName) match {
                        case Full(_) ⇒ Full(memberName)
                        case e : EmptyBox ⇒ e
                    }
                }
            }
            case _ ⇒ Failure(s"linking non-Cfs file as member '$name' is unsupported")
        }
    }

    /**
     * Unlink a member of this container. The member may continue to exist if there are
     * other links to it. And even if there are no other links, the member will not be
     * deleted until the last open file handle for it is closed.
     *
     * The caller should have file handles for this container and the member open, and
     * both associated Vnodes should be write-locked.
     *
     * @param name the member name
     * @param member the member Vnode
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container.
     */
    override def unlink(name : String, member : Vnode) : Box[(Boolean, List[String])] = {
        member match {
            case cfsmember : CfsVnode ⇒
                this.withWriteLock { () ⇒
                    cfsmember.withWriteLock { () ⇒
                        DbManager unlink (this, name, cfsmember)
                    }
                }
            case _ ⇒ Failure(s"cannot unlink non-Cfs file")
        }
    }

    /**
     * Check whether this container contains a member with a specified name.
     *
     * @param member the name of the potential member
     * @return true if the principal can ascertain the existence of the member,
     *         false otherwise
     */
    override def exists_?(member : String) : Boolean = withReadLock { () ⇒
    // lookup() will release this node if it finds the member, so acquire it
    // one more time to ensure a non-zero count.
        this.acquire
        (DbManager lookup (this, member)) map { mnode ⇒
        // lookup() will acquire the member Vnode it found. But we just
        // wanted to know if it exists, so release it.
            mnode.release
            true
        } openOr {
            // The lookup failed, so release this Vnode to make up for the
            // acquire we did before the lookup.
            this.release
            false
        }
    }

    /**
     * Get the names of all the members in this container. Optionally, only
     * members of a given MIME type can be returned.
     *
     * @param mimeType an optional MIME type to select only members of this type
     * @return the boxed member names, with an empty sequence indicating there are none.
     *         Failure on error.
     */
    override def getMembers(mimeType : Option[String]) : Box[Seq[String]] = withReadLock { () ⇒
        DbManager members this map { list ⇒
            mimeType match {
                case Some(mt) ⇒
                    list filter { name ⇒
                        this lookup (name, true) match {
                            case Full(vnode : CfsVnode) ⇒
                                val keep = vnode.getMimeType == mt
                                vnode.release
                                keep
                            case Full(other) ⇒
                                other.release
                                false
                            case _ ⇒ false
                        }
                    }
                case None ⇒ list
            }
        }
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
     * @param name the member name
     * @return a boxed Vnode for the member if successful, Empty if the member
     *         does not exist, or Failure for an error, including access denied
     */
    override def lookup(name : String, keepParent : Boolean = false) : Box[Vnode] = {
        withReadLock { () ⇒
            if (keepParent) this.acquire
            DbManager lookup (this, name) ifnot { () ⇒
                if (keepParent) this.release
            }
        }
    }

    /**************** End VDirNode implementation ***********************************/

    /**************** Begin CfsVnode-specific implementation ************************/

    /**
     * Check whether a given Vnode is contained by this Vnode. If it is,
     * return the name it has in this container.
     *
     * @param vnode the possible member Vnode
     * @return a boxed string containing the member name if it is a member, Empty
     *         if the Vnode is not a member, or a Failure on error
     */
    def getMemberName(vnode : CfsVnode) : Box[String] = withReadLock { () ⇒
        DbManager getMemberName (this, vnode)
    }

    /**
     * Find all parent containers of this Vnode. Any returned containers are
     * acquired.
     *
     * @return a boxed list of tuples of (parent Vnode, member name), where member
     *         name is the name given this Vnode within the parent container.
     */
    def findParents : Box[List[(CfsVnode, String)]] = withReadLock { () ⇒
        DbManager findContainers this
    }

    /**
     * Find a path through which a given principal can reference the file associated with
     * this Vnode.
     *
     * @param principal the principal
     * @return a boxed VPath if successful, Failure on error
     */
    def findPath(principal : Principal) : Box[CfsAbsolutePath] = withReadLock { () ⇒ DbManager findPath (this, principal) }

    /**
     * Find all the paths to the file associated with this Vnode that can be accessed
     * by a given principal.
     *
     * @param principal the principal
     * @return a boxed list of CfsPaths if successful, Failure on error
     */
    def findPaths(principal : Principal) : Box[List[CfsAbsolutePath]] = withReadLock { () ⇒ DbManager findPaths (this, principal) }

    /**
     * Get the MIME type of this file from its Resource.
     *
     * @return the MIME type of this file
     */
    def getMimeType : String = resource.getMimeType

    def getResource : Resource = resource

    def getResourceId : ResourceId = resource.getSafeKey

    def getOwnerId : ResourceId = resource.getOwnerId

    /** Called when Vnode is being flushed from cache */
    def invalidate() : Unit = {}

    /**
     * Check whether this file is special, which is true if it has an associated
     * MimeTypeHandler.
     *
     * @return true if the file has a MimeTypeHandler
     */
    def isSpecial_? : Boolean

    private var _policyRefs : Option[List[PolicyRef]] = None

    private def getPoliciesUnlocked : List[PolicyRef] = {
        _policyRefs match {
            case Some(list) ⇒ list
            case None ⇒
                val list = PolicyRef getPolicies getResourceId
                _policyRefs = Some(list)
                list
        }
    }

    def getPolicyRefs : List[PolicyRef] = withReadLock { () ⇒ _policyRefs } match {
        case Some(list) ⇒ list
        case None ⇒ withWriteLock { () ⇒ getPoliciesUnlocked }
    }

    def getPolicies : List[ResourceId] = getPolicyRefs map (_.getPolicy)

    def addPolicy(policy : ResourceId) : List[PolicyRef] = withWriteLock { () ⇒
        val prefs = getPoliciesUnlocked
        val newprefs =
            if (prefs exists (_.getPolicy == policy)) prefs
            else {
                PolicyRef make (getResourceId, policy) match {
                    case Full(pref) ⇒
                        CacheFilter remove getResourceId
                        pref :: prefs
                    case e : EmptyBox ⇒
                        Log.error(s"failed to add policy id ${policy.id} to resource ${getResourceId.id}")
                        prefs
                }
            }
        _policyRefs = Some(newprefs)
        newprefs
    }

    def removePolicy(policy : ResourceId) : List[PolicyRef] = withWriteLock { () ⇒
        val (rempref, newprefs) = getPoliciesUnlocked partition (_.getPolicy == policy)
        rempref match {
            case Nil ⇒ newprefs
            case head :: tail ⇒
                CacheFilter remove getResourceId
                head.delete_!
                _policyRefs = Some(newprefs)
                newprefs
        }
    }

    def removeAllPolicies() : Unit = {
        withWriteLock { () ⇒
            val resid = getResourceId
            CacheFilter remove resid
            PolicyRef removeResource resid
            _policyRefs = Some(Nil)
        }
    }

    /**
     * Get a list of rights that a given principal is granted for this Vnode.
     * This is based on the policies currently applied to the resource, the assignment
     * of principals to roles specified by those policies, and the rights associated
     * with each such role held by the given principal. This includes role assignments
     * to ancestors of the given principal, so that, for example, a role assigned to
     * a user group will apply to individual members of the group.
     *
     * @param principal the principal whose rights to the resource are needed
     * @return a list of rights
     */
    def getRights(principal : Principal) : List[RightId] = {
        // Loop over the policies from the given Vnode
        getPolicyRefs flatMap { pref ⇒
        // Get the PolicyNode of the referenced policy
            Cfs.withPolicyNode (pref.getPolicy) { pnode ⇒
            // Get the rights that this policy grants to the specified principal
                Full(pnode getRightsOf principal)
            } openOr Nil
        }
    }
}

/**
 * Companion object for CfsVnode class.
 */
object CfsVnode {

    /**
     * Restrictions on what parent container types certain member types can have.
     */
    val parentRestrictions = Map("choice/user" → List("choice/group"))

    /**
     * Restrictions on what member types some container types can have.
     */
    val childRestrictions = Map("choice/group" → List("choice/user", "choice/group"),
                                   "choice/role" → List("choice/right"))
}
