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
package choice.access

import java.util.concurrent.locks.ReentrantReadWriteLock

import choice.core.CacheFilter
import choice.fs._
import choice.fs.vfs.CanHazMap
import choice.lib.Glob
import choice.model._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers.tryo

import scala.util.matching.Regex

/**
 * This associates an access control policy with a file resource. Such a policy
 * applies regardless of the path used to access the file.
 */
class PolicyRef extends LongKeyedMapper[PolicyRef] with IdPK {

    def getSingleton : PolicyRef.type = PolicyRef
    
    /* Reference to a file resource to which this policy is applied */
    object file extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }
    
    /* Reference to a policy resource that defines this policy */
    object policy extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    /**
     * Return all the role assignments for the policy associated with this entry.
     *
     * @return a list of role assignments
     */
    def getRoleAssignments : List[RoleAssn] = {
        RoleAssn.findAll(By(RoleAssn.policy, this.policy.get), OrderBy(RoleAssn.role, Ascending))
    }

    def getPolicy : ResourceId = ResourceId(policy.get)
}

object PolicyRef extends PolicyRef with LongKeyedMetaMapper[PolicyRef] {

    def make(file : ResourceId, policy : ResourceId) : Box[PolicyRef] = {
        tryo {
            PolicyRef.create.file(file).policy(policy).saveMe()
        }
    }

    /**
     * Return a list of all the PolicyRef instances attached to a given file.
     *
     * @param resourceId the resource id of the file
     * @return a list of PolicyRefs for the file
     */
    def getPolicies(resourceId : Long) : List[PolicyRef] = {
        PolicyRef.findAll(By(PolicyRef.file, resourceId))
    }
    
    def getPolicies(resource : Resource) : List[PolicyRef] = {
        getPolicies(resource.id.get)
    }

    /**
     * Remove all references to a given policy.
     *
     * @param policy the resource id of a policy
     */
    def removePolicy(policy : ResourceId) : Unit = {
        PolicyRef bulkDelete_!! By(PolicyRef.policy, policy.id)
    }

    /**
     * Remove all policy associations with a given file.
     *
     * @param resource the resource id of a file
     */
    def removeResource(resource : ResourceId) : Unit = {
        PolicyRef bulkDelete_!! By(PolicyRef.file, resource.id)
    }
}

/** Define file path pattern types */
object PathPatternType extends Enumeration {
    type PathPatternType = Value
    /** Globbing syntax, version 1 */
    val GLOBV1 : PathPatternType.Value = Value(0)
    /** Scala Regex syntax */
    val REGEX : PathPatternType.Value = Value(1)
    /** Path prefix match */
    val PREFIX : PathPatternType.Value = Value(2)
}

class PathPolicy extends LongKeyedMapper[PathPolicy] with IdPK with CanHazMap {
    def getSingleton : PathPolicy.type = PathPolicy

    /**
     * The principal who applied this policy. The policy will apply only to files
     * which match the path pattern and for which this principal has permission
     * to set access control policies. Currently this requires the principal to
     * be either an owner of the file or an administrator.
     */
    object principal extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    /**
     * The policy to be applied to files matching the path pattern.
     */
    object policy extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    /** The path pattern type */
    object ptype extends MappedEnum(this, PathPatternType) {
        override def dbNotNull_? = true
    }

    object mtprimary extends MappedString(this, 128)

    object mtsubtype extends MappedString(this, 128)

    /** The path pattern */
    object pattern extends MappedString(this, 1024) {
        override def dbNotNull_? = true
    }

    def matches(path : String, mimetype : String) : Boolean = {
        import choice.access.PathPatternType._

        // See if there's a MIME type constraint at the beginning of the pattern.
        // Such a constraint has a pattern for the MIME type between '%' characters
        val ok = tryo {
            val pt = mtprimary.get
            val st = mtsubtype.get
            if ((pt != null) || (st != null)) {
                val mt = mimetype split '/'
                ((pt == null) || (mt(0) == pt)) && ((st == null) || (mt(1) == st))
            }
            else true
        } openOr false

        // If there was no MIME type constraint, or there was one that matched,
        // see if the file path matches the rest of the pattern
        val pstring = pattern.get
        if (ok) {
            (pstring == null) || (pstring == "") || {
                ptype.get match {
                    case GLOBV1 ⇒ Glob.matches(pstring, path)
                    case REGEX ⇒
                        tryo {
                            new Regex(pstring) findPrefixMatchOf path match {
                                case Some(m) ⇒ m.after.length() == 0
                                case None ⇒ false
                            }
                        } openOr false
                    case PREFIX ⇒
                        (path startsWith pstring) && {
                            (pstring == "/") || {
                                val len = pstring.length
                                len >= path.length || (path charAt len) == '/'
                            }
                        }
                }
            }
        }
        else false
    }

    def mtRender : String = {
        val pt = mtprimary.get
        val st = mtsubtype.get
        s"""${if (pt == null || pt == "") "*" else pt}/${if (st == null || st == "") "*" else st}"""
    }

    def asMap : Map[String, Any] = {
        val principalMap = CfsPrincipal(ResourceId(principal.get)).asMap
        val policyId = policy.get
        val policyMap = Cfs open (CfsVFileId(ResourceId(policyId)), SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(pfile : Policy) ⇒
                val result = pfile.asMap
                pfile close ()
                result
            case Full(vfile) ⇒
                vfile close ()
                Map[String, Any]("id" → policyId, "status" → -1, "msg" → "not a policy")
            case Empty ⇒ Map[String, Any]("id" → policyId, "status" → -1, "msg" → "does not exist")
            case f : Failure ⇒ Map[String, Any]("id" → policyId, "status" → -1, "msg" → f.messageChain)
        }
        Map("id" → id.get, "principal" → principalMap, "policy" → policyMap,
            "ptype" → ptype.get.toString, "pattern" → pattern.get, "mimetype" → mtRender)
    }
}

object PathPolicy extends PathPolicy with LongKeyedMetaMapper[PathPolicy] {

    import choice.access.PathPatternType._

    private val _rwlock = new ReentrantReadWriteLock()
    private var _allEntries : Option[List[PathPolicy]] = None

    /**
     * Get all PathPolicy entries.
     *
     * This maintains a cache of all PathPolicies, refreshing it when necessary.
     *
     * @return a list of all PathPolicy table entries
     */
    def getAllEntries: List[PathPolicy] = {
        _rwlock.readLock().lock()
        val fresult = _allEntries match {
            case Some(list) ⇒ list
            case None ⇒
                _rwlock.readLock().unlock()
                _rwlock.writeLock().lock()
                val result = _allEntries match {
                    case None ⇒
                        val list = PathPolicy findAll ()
                        _allEntries = Some(list)
                        list
                    case Some(list) ⇒ list
                }
                _rwlock.readLock().lock()
                _rwlock.writeLock().unlock()
                result
        }
        _rwlock.readLock().unlock()
        fresult
    }

    /**
     * Get PathPolicy entries that could apply to a file with a specified owner.
     *
     * Each PathPolicy entry contains a principal, which is usually the principal
     * of the user who created the entry. This principal must have the right to
     * set access control on a file owned by the specified owner, in order for its
     * entry to be included in the result.
     *
     * @param owner the owner of a file
     * @return list of PathPolicy entries for the given owner
     */
    def getEntriesForOwner(owner : Principal) : List[PathPolicy] = {
        val ownerId = owner.getPrincipalId
        // Scan all the PathPolicy entries
        (getAllEntries foldLeft (Nil : List[PathPolicy])) { (list, pp) ⇒
            // If the principal is the file owner, include the entry
            if (pp.principal.get == ownerId.id) pp :: list
            else {
                // Otherwise construct a Principal object for the PathPolicy principal
                val pprincipal = CfsPrincipal(ResourceId(pp.principal.get))
                // If the PathPolicy principal is an administrator, include the entry
                if (pprincipal.isSystemAdmin_?) pp :: list
                else {
                    // Last chance - check if the owner is a group and the PathPolicy
                    // principal is a member of the group.
                    if (owner.isGroup_? && (pprincipal isDescendantOf ownerId)) {
                        pp :: list
                    }
                    else list
                }
            }
        }
    }

    /**
     * Get the PathPolicy entries that apply to a specified file.
     *
     * @param file the file for which PathPolicy entries are needed
     * @return a list of matching PathPolicy entries
     */
    def getEntriesForFile(file : CfsFile) : List[PathPolicy] = {
        getEntriesForOwner (CfsPrincipal(file.getVnode.getOwnerId)) filter { pp ⇒
            pp matches (file.getPath.toString, file.getVnode.getMimeType)
        }
    }

    /**
     * Set a new path policy. The policy will be effective only for files which match
     * the specified path pattern, and for which the specified principal has the right
     * to set access control. No conditions are imposed on the principal for the
     * purpose of setting the path policy. Also nothing prevents a duplicate path
     * policy entry from being defined, though there is no good reason to do that.
     *
     * If exactly the same path policy by the same principal already exists, it is
     * simply returned and not duplicated.
     *
     * @param policyId the policy to be applied to files matching the pattern
     * @param pattern the pattern string for matching file paths
     * @param ptype the type of the pattern string, GLOBV1 or REGEX
     * @param mimetype an optional MIME type string, which must also match
     * @param by the principal associated with the new path policy
     * @return a boxed PathPolicy if successful
     */
    def set(policyId : ResourceId, pattern : String, ptype : PathPatternType,
            mimetype : Option[String], by : Principal) : Box[PathPolicy] = {
        val validPattern = ptype match {
            case PREFIX ⇒ Cfs.withValidPath(pattern)(p ⇒ Full(p.toString))
            case GLOBV1 ⇒ tryo(new Regex(Glob.globToRegex(pattern))) map (_ ⇒ pattern)
            case REGEX ⇒ tryo(new Regex(pattern)) map (_ ⇒ pattern)
        }
        validPattern match {
            case Full(pat) ⇒
                val (pt, st) = mimetype match {
                    case Some(mt) ⇒
                        val mts = mt split '/'
                        if (mts.length == 2) (mts(0), mts(1)) else (mts(0), null)
                    case None ⇒ (null, null)
                }
                _rwlock.writeLock().lock()
                val result = tryo {
                    val exists = PathPolicy find (
                        By(PathPolicy.principal, by.getPrincipalId.id),
                        By(PathPolicy.policy, policyId.id),
                        By(PathPolicy.pattern, pat),
                        By(PathPolicy.ptype, ptype)
                    )
                    exists flatMap { pp ⇒
                        if (pp.mtprimary.get == pt && pp.mtsubtype.get == st) exists else Empty
                    } openOr {
                        _allEntries = None
                        PathPolicy.create
                            .principal(by.getPrincipalId)
                            .policy(policyId)
                            .mtprimary(pt)
                            .mtsubtype(st)
                            .pattern(pat).ptype(ptype)
                            .saveMe()
                    }
                }
                _rwlock.writeLock().unlock()
                result
            case _ : EmptyBox ⇒ Failure(s"invalid path pattern '$pattern'")
        }
    }

    /**
     * Remove a PathPolicy. The specified principal must be the principal associated with the
     * identified PathPolicy, or a system administrator, or if the PathPolicy principal is a
     * group, a descendant of that group. A PathPolicy is identified by its id field.
     *
     * @param id the id of a PathPolicy entry
     * @param by the principal responsible for this operation
     * @return a boxed value of true if the entry was deleted, a boxed value of false if the
     *         delete was attempted but failed, or a Failure if the identified entry does not
     *         exist or the given principal lacks permission to delete it.
     */
    def remove(id : Long, by : Principal) : Box[Boolean] = {
        CacheFilter clear ()
        _rwlock.writeLock().lock()
        val result = PathPolicy findByKey id match {
            case Full(pp) ⇒
                if ((pp.principal.get == by.getPrincipalId.id) || by.isSystemAdmin_? || {
                    val pprincipal = CfsPrincipal(ResourceId(pp.principal.get))
                    pprincipal.isGroup_? && (by isDescendantOf pprincipal.getPrincipalId)
                }) tryo { pp.delete_! }
                else Failure(s"insufficient permission to remove PathPolicy")
            case Empty ⇒ Failure(s"no PathPolicy with id $id exists")
            case f : Failure ⇒ f
        }
        _allEntries = None
        _rwlock.writeLock().unlock()
        result
    }

    /**
     * Return a list of all the path policies that could be removed by a specified principal.
     *
     * @param principal the principal who might remove path policies
     * @return a list of PathPolicy entries that could be removed by the specified principal
     */
    def list(principal : Principal) : List[PathPolicy] = {
        getAllEntries filter { pp ⇒
            (pp.principal.get == principal.getPrincipalId.id) || principal.isSystemAdmin_? || {
                val pprincipal = CfsPrincipal(ResourceId(pp.principal.get))
                pprincipal.isGroup_? && (principal isDescendantOf pprincipal.getPrincipalId)
            }
        }
    }
}

/**
 * Assign a role to a principal for a particular policy. A policy consists
 * of any number of these entries.
 */
class RoleAssn extends LongKeyedMapper[RoleAssn] with IdPK {
    
    def getSingleton : RoleAssn.type = RoleAssn
    
    /* Reference to the policy resource that includes this role assignment */
    object policy extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }
    
    /* Reference to the resource of a principal being assigned the role */
    object principal extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    /* The role being assigned to the principal */
    object role extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    def getSafeKey : RoleAssnId = RoleAssnId(id.get)

    def getPolicy : ResourceId = ResourceId(policy.get)

    def getPrincipal : ResourceId = ResourceId(principal.get)

    def getRole : RoleId = RoleId(role.get)
}

object RoleAssn extends RoleAssn with LongKeyedMetaMapper[RoleAssn] {

    def make(policyId : ResourceId, principal : ResourceId, roleId : RoleId) : Box[RoleAssn] = {
        tryo {
            RoleAssn.create.policy(policyId).principal(principal).role(roleId).saveMe()
        }
    }

    /**
     * Remove all the role associations for a given policy.
     *
     * @param policy the policy resource id
     */
    def removePolicy(policy : ResourceId) : Unit = {
        RoleAssn bulkDelete_!! By(RoleAssn.policy, policy.id)
    }
}

class PolicyNode(resource : Resource) extends CfsSpecialNode(resource) with AtomicDataNode[PolicyNode] {
    import choice.access.PolicyNode.Log

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[Policy] = {
        Full(new Policy (path, principal, this))
    }

    private var _roleAssignments : Option[List[RoleAssn]] = None

    private def getRoleAssignmentsUnlocked : List[RoleAssn] = _roleAssignments match {
        case Some(list) ⇒ list
        case None ⇒
            val list = Policy getRoleAssignments getResourceId
            _roleAssignments = Some(list)
            list
    }

    def getRoleAssignments : List[RoleAssn] = {
        withReadLock { () ⇒ _roleAssignments } match {
            case Some(list) ⇒ list
            case None ⇒ withWriteLock { () ⇒ getRoleAssignmentsUnlocked }
        }
    }

    def addRoleAssignment(principal : ResourceId, roleId : RoleId) : List[RoleAssn] = withWriteLock { () ⇒
        val list = getRoleAssignmentsUnlocked
        if (list exists (ra ⇒ ra.getRole == roleId && ra.getPrincipal == principal)) list
        else {
            RoleAssn.make(getResourceId, principal, roleId) map { assn ⇒
                val newlist = assn :: list
                _roleAssignments = Some(newlist)
                newlist
            } openOr list
        }
    }

    def removeRoleAssignment(principal : ResourceId, roleId : RoleId) : List[RoleAssn] = withWriteLock { () ⇒
        val (remove, remaining) = getRoleAssignmentsUnlocked partition { assn ⇒
            assn.getPrincipal == principal && assn.getRole == roleId
        }
        remove match {
            case Nil ⇒ remaining
            case head :: _ ⇒
                head.delete_!
                _roleAssignments = Some(remaining)
                remaining
        }
    }

    def removeAllRoleAssignments() : Unit = withWriteLock { () ⇒
        RoleAssn removePolicy getResourceId
        _roleAssignments = Some(Nil)
    }

    /**
     * Get the rights that this policy grants to a specified principal.
     *
     * @param principal the principal for whom the rights are needed
     * @return a list of the right ids for the given principal
     */
    def getRightsOf(principal : Principal) : List[RightId] = {
        // Get the role assignments of the policy, and select the ones which apply
        // to the given principal.
        val roleIds = getRoleAssignments filter (principal isDescendantOf _.getPrincipal) map (_.getRole)
        // Convert each role to rights, by getting the RoleNode and querying its rights
        roleIds flatMap { id ⇒
            Cfs.withRoleNode (id) { rnode ⇒
                Full(rnode.getRightDefs flatMap (Module getRightId _.name))
            } match {
                case Full(list) ⇒ list
                case e : EmptyBox ⇒
                    Log.error(s"getRights error", e)
                    Nil
            }
        }
    }
}

object PolicyNode {
    protected val Log = Logger("choice.access.PolicyNode")
}

class Policy(path : CfsAbsolutePath, principal : Principal, vnode : PolicyNode)
    extends CfsSpecial(path, principal, vnode) with AtomicDataFile[Policy, PolicyNode] {
    import choice.access.Policy.Log

    override def getVnode : PolicyNode = vnode

    def getBytes : Box[Array[Byte]] = (Policy canReadDescription this) (() ⇒ getBytesUnchecked)

    def putBytes(data : Array[Byte]) : Box[Boolean] = (Policy canWriteDescription this) (() ⇒ putBytesUnchecked (data))

    /**
     * Get a list of the role assignments of a policy. The list contains
     * 3-tuples consisting of a principal name, principal resource id,
     * and role id of a role granted to that principal.
     *
     * @return a list of 3-tuples as described above
     */
    def listRoles : List[(String, ResourceId, RoleId)] = {
        getVnode.getRoleAssignments flatMap { assn ⇒
            (Cfs withVnode assn.getPrincipal) {
                case pnode : IsPrincipal ⇒
                    val principal = pnode.getSelfPrincipal
                    Full((principal.getPrincipalName, principal.getPrincipalId, assn.getRole))
                case _ ⇒ Empty
            } match {
                case Full(tuple) ⇒ List(tuple)
                case e : EmptyBox ⇒
                    Log.error("listRoles error", e)
                    Nil
            }
        }
    }

    /**
     * Apply this policy to a set of files identified by a file specification
     * string. The string may be the full path to a single file, or the path of
     * a container file with \/\* or \/\*\* appended. The wildcard suffixes indicate
     * the container and its members, or the container and all its descendants,
     * respectively. No action is taken if the policy is already associated with a
     * file. Otherwise an entry is created in the PolicyRef table to associate the
     * policy with each file.
     *
     * This may not affect rights associated with currently open handles for a file.
     * But it will affect subsequently opened handles for a file.
     *
     * @param fspec a specification of the files to which the policy is to be applied.
     * @param principal the principal to be used when accessing files referenced by fspec
     * @return a list of the file paths matched by fspec, along with a boxed Boolean
     *         indicating whether the policy was applied (true), or was already on
     *         the file (false), or a Failure
     */
    def protect(fspec : String, principal : Principal) : List[(String, Box[Boolean])] = {
        val policyId = getResourceId
        def helper(path : String) : Box[Boolean] = {
            Cfs.withExistingFile(path, principal, CfsOpenOptions.Default) {
                case dbfile : CfsFile ⇒
                    val targetVnode = dbfile.getVnode
                    val ownerId = targetVnode.getOwnerId
                    // Limit policy changes to file owners and administrators
                    if ((principal.getPrincipalId == ownerId) ||
                        (GlobalConfig isSystemAdmin_? principal) || (principal isDescendantOf ownerId)) {
                        val targetPolicies = targetVnode.getPolicyRefs
                        val alreadyApplied = targetPolicies exists (_.getPolicy == policyId)
                        if (alreadyApplied) Full(false)
                        else {
                            targetVnode addPolicy policyId
                            Full(true)
                        }
                    }
                    else Failure(s"access denied")
            }
        }
        val fileList = Cfs expandFileSpec (fspec, principal)
        (fileList foldLeft List[(String, Box[Boolean])]()) { (list, path) ⇒
            (path, helper(path)) :: list
        }
    }

    /**
     * Remove the association of this policy with a specified file. No action
     * is taken if the policy is not associated with the file. Otherwise the
     * association is removed by deleting an entry in the PolicyRef table. Since this
     * may affect the rights of the principal associated with the dbfile file handle,
     * the dbfile file is reopened to obtain a new handle, and the argument handle
     * is closed.
     *
     * @param fspec a specification of the files from which the policy is to be removed.
     * @param principal the principal to be used when accessing files referenced by fspec
     * @return a list of the file paths matched by fspec, along with a boxed Boolean
     *         indicating whether the policy was removed (true), or wasn't on
     *         the file (false), or a Failure
     */
    def unprotect(fspec : String, principal : Principal) : List[(String, Box[Boolean])] = {
        val policyId = getResourceId
        def helper(path : String) : Box[Boolean] = {
            Cfs.withExistingFile(path, principal, CfsOpenOptions.Default) {
                case dbfile : CfsFile ⇒
                    val targetVnode = dbfile.getVnode
                    val ownerId = targetVnode.getOwnerId
                    // Limit policy changes to file owners and administrators
                    if ((principal.getPrincipalId == ownerId) ||
                        (GlobalConfig isSystemAdmin_? principal) || (principal isDescendantOf ownerId)) {
                        val targetPolicies = targetVnode.getPolicyRefs
                        targetPolicies find (_.getPolicy == policyId) match {
                            case Some(_) ⇒
                                targetVnode removePolicy policyId
                                Full(true)
                            case None ⇒ Full(false)
                        }
                    }
                    else Failure(s"access denied")
            }
        }
        val fileList = Cfs expandFileSpec (fspec, principal)
        (fileList foldLeft List[(String, Box[Boolean])]()) { (list, path) ⇒
            (path, helper(path)) :: list
        }
    }

    /**
     * Set the description of this policy as the content of the policy file. This
     * does not affect the role assignments of the policy.
     *
     * @param desc a description of the policy for human consumption
     * @return the boxed policy file if successful
     */
    def describePolicy(desc : String) : Box[Policy] = {
        (putString (desc) map (_ ⇒ this)) ?~! s"failed to set description in policy $getPath"
    }

    /**
     * Add roles to this policy. The caller provides a list of pairs of principal id
     * and role to be added to a specified policy. This information is added to the
     * RoleAssn table. Any role assignments already included in the policy are ignored.
     *
     * @param list a list of tuples containing the resource id of a principal, and
     *             the id of a role to be granted that principal
     * @return this boxed policy file
     */
    def addRole(list : List[(ResourceId, RoleId)]) : Box[Policy] = {
        // Add any new role assignments
        list foreach { pair ⇒
            val (principalId, roleId) = pair
            getVnode addRoleAssignment (principalId, roleId)
        }
        Full(this)
    }

    /**
     * Remove role assignments from this policy. The caller provides a list of pairs of
     * principal id and role to be removed from the policy. Any entries in the list
     * which do not represent a role assignment associated with the policy are ignored.
     *
     * @param list a list of tuples containing the resource id of a principal, and
     *             the id of a role to be revoked for that principal
     * @return the boxed policy file
     */
    def removeRole(list : List[(ResourceId, RoleId)]) : Box[Policy] = {
        list foreach { pair ⇒
            val (principalId, roleId) = pair
            getVnode removeRoleAssignment (principalId, roleId)
        }
        Full(this)
    }

}

object Policy extends MimeTypeHandler {

    override protected val Log = Logger("choice.model.Policy")

    override def getName = "Policy File Types"
    override def getSchemas : List[BaseMetaMapper] = List(PolicyRef, PathPolicy, RoleAssn)

    override def getMimeType = "choice/policy"

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override val getRights : List[RightDef] = List(
        RightDef("create_policy", "policy files", "create a policy"),
        RightDef("unlink_policy", "policy_files", "remove a policy"),
        RightDef("read_policy_desc", "policy files", "read policy description"),
        RightDef("write_policy_desc", "policy files", "write policy description"),
        RightDef("read_policy", "policy files", "read policy contents"),
        RightDef("add_to_policy", "policy files", "add principal/rights mapping to policy"),
        RightDef("remove_from_policy", "policy files", "remove principal/rights mapping from policy")
    )

    override def isContainer_? = false

    override def instantiate(resource : Resource) : Box[PolicyNode] = {
        if (isType_?(resource)) Full(new PolicyNode(resource))
        else Failure(s"resource id ${resource.getSafeKey.id} is not a policy")
    }

    /**
     * This method is called by DbManager when it is about to delete a file with a MIME
     * type associated with this MimeTypeHandler. It gives the MimeTypeHandler an
     * opportunity to veto the delete, or to do other cleanup operations, such as
     * removing non-filesystem references to the file.
     *
     * Note that this is called on the DbManager thread, so it should only use the
     * dbPlugin interface to access DbManager functions.
     *
     * @param vnode the vnode of the file about to be deleted
     * @return a boxed value of true if the delete should proceed, otherwise a
     *         boxed false or EmptyBox
     */
    override def delete(vnode : CfsVnode) : Box[Boolean] = vnode match {
        case pnode : PolicyNode ⇒
            val policyId = pnode.getResourceId
            // Remove all the references to the policy being deleted. First delete
            // any references in cached Vnodes. Then delete any remaining in the database.
            dbPlugin forAllVnodes { vnode ⇒ vnode removePolicy policyId }
            PolicyRef removePolicy policyId
            // Remove all the role associations belonging to this policy
            pnode removeAllRoleAssignments ()
            // Do the common delete code
            super.delete(vnode)
        case _ ⇒ Failure(s"Policy delete called with non-PolicyNode")
    }

    def invalidate(resid : ResourceId, mtid : MimeTypeId) : Unit = {}

    def getPolicies(resid : ResourceId) : Array[ResourceId] = {
        (PolicyRef.findAll(By(PolicyRef.file, resid)) map (pref ⇒ ResourceId(pref.policy.get))).toArray
    }

    def getRoleAssignments(policy : ResourceId) : List[RoleAssn] = {
        RoleAssn.findAll(By(RoleAssn.policy, policy))
    }

    /**
     * Create a new, empty policy file at a specified path. The policy file will be
     * owned by the given principal. The folder in which the policy is to be created
     * must already exist, and the principal must have the right to add a member to it.
     *
     * The real significance of a policy file is determined by entries in the RoleAssn
     * table which reference its resource id. These are the role assignments of the
     * policy, which can be added or removed only via the PolicyManager.
     *
     * A policy file can contain data, which is just a textual description of the policy.
     *
     * @param path the path at which to create the policy file
     * @param by the principal creating the file
     * @return a boxed Policy file
     */
    def createPolicy(path : CfsPath, by : Principal) : Box[Policy] = {
        val ppath = path.getParent
        Cfs.withExistingFile (ppath, by) {
            case dbcont : CfsDirFile ⇒
                dbcont create (path.getFileName.toString, Policy.getMimeType, CfsCreateOptions.Default) match {
                    case Full(policy : Policy) ⇒ Full(policy)
                    case Full(dbfile) ⇒
                        dbfile close ()
                        Failure("createFile did not return a Policy")
                    case e : EmptyBox ⇒ e
                }
            case _ : CfsFile ⇒ Failure(s"$ppath is not a container")
        }
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_policy")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_policy")

    val canReadDescription = AnyRightsCheck("read_policy_desc", "read_policy", "add_to_policy", "remove_from_policy")
    val canWriteDescription = AnyRightsCheck("write_policy_desc", "add_to_policy", "remove_from_policy")
}
