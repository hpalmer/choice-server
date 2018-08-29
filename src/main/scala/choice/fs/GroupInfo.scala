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
  * Implement the "choice/group" file type, which represents a user group.
  *
  * @author Howard Palmer
  */
package choice.fs

import choice.access._
import choice.actor.DbManager
import choice.fs.vfs.{CanHazMap, VFile}
import choice.model.{GlobalConfig, MimeType, Resource}
import net.liftweb.common._
import net.liftweb.json.DefaultFormats
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers.millis

import scala.Predef._
import scala.annotation.tailrec

class GroupCreateOptions(val gdesc : Option[GroupDesc], override val dataSeqnum : Long,
                         override val ctime : Long, override val altid : Long,
                         override val replace : Boolean)
    extends CfsCreateOptions(dataSeqnum, ctime, altid, replace)

object GroupCreateOptions {
    def apply(gdesc : Option[GroupDesc] = None, dataSeqnum : Long = 0L, ctime : Long = millis,
              altid : Long = 0L, replace : Boolean = false) : GroupCreateOptions = {
        new GroupCreateOptions(gdesc = gdesc, dataSeqnum = dataSeqnum, ctime = ctime, altid = altid, replace = replace)
    }
}

/**
 * This consolidates information about a user group.
 * The companion object generally should be used to construct instances, starting
 * from different sets of information.
 */
class GroupInfo(path : CfsAbsolutePath, principal : Principal, vnode : GroupNode)
		extends CfsFolder(path, principal, vnode)
        with JsonDataFile[GroupInfo, GroupNode, GroupDesc] with IsPrincipal {
    import choice.fs.GroupInfo._
    implicit val formats = DefaultFormats

    /**
     * Get the principal represented by this user group. This is different than the
     * principal in the GroupInfo constructor arguments, which is the principal
     * used to open the group.
     *
     * @return the Principal represented by this user group
     */
    def getSelfPrincipal : CfsPrincipal = CfsPrincipal(this)

    /**
     * Get GroupInfo instances for each child user group of this group, that is,
     * user groups which are immediate members of this group.
     *
     * @return	a list of GroupInfo instances
     */
    def getGroups : List[GroupInfo] = canListMember (this) { () ⇒
        vnode withReadLock { () ⇒ DbManager lookupMembers (vnode, GroupInfo.getMimeTypeId :: Nil) } match {
            case Full(list) ⇒
                val outlist = filterMembers(list, Nil) { (name, vn) ⇒
                    vn match {
                        case gnode : GroupNode ⇒
                            gnode open (path / name, principal, CfsOpenOptions.Default) match {
                                case Full(ginfo : GroupInfo) ⇒ Some(ginfo)
                                case Full(other) ⇒
                                    other close ()
                                    None
                                case _ : EmptyBox ⇒ None
                            }
                        case onode ⇒
                            onode.release
                            None
                    }
                }
                Full(outlist)
            case e : EmptyBox ⇒ e
        }
    } openOr Nil

    /**
     * Get GroupInfo instances for all descendant user groups of this group.
     *
     * @return	a list of GroupInfo instances
     */
    def getGroups_! : List[GroupInfo] = canListMember (this) { () ⇒
        def helper(ingroups : List[GroupInfo], acc : List[GroupInfo]) : List[GroupInfo] = {
            ingroups match {
                case Nil ⇒ acc
                case head :: tail ⇒
                    helper (tail ++ head.getGroups, head :: acc)
            }
        }
        Full(helper(getGroups, Nil))
    } openOr Nil

    def getUsers : List[UserInfo] = canListMember (this) { () ⇒
        vnode withReadLock { () ⇒ DbManager lookupMembers (vnode, UserInfo.getMimeTypeId :: Nil) } match {
            case Full(list) ⇒
                val outlist = filterMembers(list, Nil) { (name, vn) ⇒
                    vn match {
                        case unode : UserNode ⇒
                            unode open (path / name, principal, CfsOpenOptions.Default) match {
                                case Full(uinfo : UserInfo) ⇒ Some(uinfo)
                                case Full(other) ⇒
                                    other close ()
                                    None
                                case _ : EmptyBox ⇒ None
                            }
                        case onode ⇒
                            onode.release
                            None
                    }
                }
                Full(outlist)
            case e : EmptyBox ⇒ e
        }
    } openOr Nil

    def getUsers_! : List[UserInfo] = {
        val result = getUsers ++ (getGroups_! flatMap { g ⇒
            val gu = g.getUsers
            g close ()
            gu
        })
        Full(result)
    } openOr Nil

    /**
     * Get a UserInfo instance for a specified username in this group.
     * This is somewhat optimized over methods that go through openUnchecked(),
     * since it knows that the username should be case-folded.
     *
     * @param username	the login username for the user
     *
     * @return	a boxed UserInfo if successful, otherwise a Failure
     */
    def findUser(username : String) : Box[UserInfo] = canListMember (this) { () ⇒
        super.openUnchecked(username.toLowerCase, getPrincipal, CfsOpenOptions.Default) match {
            case Full(uinfo : UserInfo) ⇒ Full(uinfo)
            case Full(ginfo : GroupInfo) ⇒
                ginfo close ()
                Failure(s"$username is a group")
            case Full(other) ⇒
                other close ()
                Failure(s"$username is not a user")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Find the specified username in this group or any of its descendants.
     *
     * @param username the username
     * @return a boxed UserInfo if username found, Empty if not found, otherwise a Failure
     */
    def findUser_!(username : String) : Box[UserInfo] = canListMember (this) { () ⇒
        def helper(group : GroupInfo) : Box[UserInfo] = {
            // Try for the user in the current group
            group findUser username match {
                case full @ Full(_) ⇒ full
                case Empty ⇒
                    // Failing that, try each of the subgroups of the current group
                    val subgroups = group.getGroups
                    val result = listhelper(subgroups)
                    // Then close all the subgroups
                    subgroups foreach (_ close ())
                    result
                case f : Failure ⇒ f
            }
        }
        // Apply helper() to a list of groups, stopping early if the user is found
        // or an error occurs
        def listhelper(glist : List[GroupInfo]) : Box[UserInfo] = {
            glist match {
                case Nil ⇒ Empty
                case head :: tail ⇒
                    helper(head) match {
                        case full @ Full(_) ⇒ full
                        case Empty ⇒ listhelper(tail)
                        case f : Failure ⇒ f
                    }
            }
        }
        helper (this)
    }

    def getGroupDesc : Box[GroupDesc] = canReadAttributes (this) { () ⇒ getData }

	def getDescription : Option[String] = {
	    getGroupDesc map ( _.description ) openOr None
	}

	def isLoginEnabled_? : Boolean = getGroupDesc map ( _.isLoginEnabled_? ) openOr false

    def isGoogleLoginEnabled_? : Boolean = getGroupDesc map ( _.isGoogleLoginEnabled_? ) openOr false

	def isSignupEnabled_? : Boolean = getGroupDesc map ( _.isSignupEnabled_? ) openOr false

    def isGoogleSignupEnabled_? : Boolean = getGroupDesc map ( _.isGoogleSignupEnabled_? ) openOr false

	def isCaptchaEnabled_? : Boolean = getGroupDesc map ( _.isCaptchaEnabled_? ) openOr false

    def isVerifyEmailEnabled_? : Boolean = getGroupDesc map ( _.isVerifyEmailEnabled_? ) openOr false

    def getRegKey : Box[String] = getGroupDesc flatMap ( _.regKey )

    def getGuestPage : Box[String] = getGroupDesc flatMap { gdesc ⇒ gdesc.guestPage }

    def getHomePage : Box[String] = getGroupDesc flatMap { gdesc ⇒ gdesc.homePage }

    /** Enable or disable logins to this group */
    def enableLogin(enable : Boolean) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            if (gdesc.isLoginEnabled_? != enable) {
                val newgdesc = gdesc setLoginEnabled enable
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    /** Allow or disable user self-registration in this group */
    def enableSignup(enable : Boolean) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            if (gdesc.isSignupEnabled_? != enable) {
                val newgdesc = gdesc setSignupEnabled enable
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    def enableCaptcha(enable : Boolean) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            if (gdesc.isCaptchaEnabled_? != enable) {
                val newgdesc = gdesc setCaptchaEnabled enable
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    def setGuestPage(page : Option[String]) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            val newgdesc = gdesc setGuestPage page
            if (!(newgdesc eq gdesc)) {
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    def setHomePage(page : Option[String]) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            val newgdesc = gdesc setHomePage page
            if (!(newgdesc eq gdesc)) {
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    def setDescription(desc : Option[String]) : Box[GroupInfo] = canWriteAttributes (this) { () ⇒
        getGroupDesc flatMap { gdesc ⇒
            val newgdesc = gdesc setDescription desc
            if (!(newgdesc eq gdesc)) {
                putData (newgdesc) map (_ ⇒ this) or
                    Failure(s"failed to update attributes for user group '$getPath'")
            }
            else Full(this)
        }
    }

    override def getVnode : GroupNode = vnode


    /**
     * Open a specified member of this container. This permits a different principal
     * to be used than was used to open this container. The supported options will
     * generally be the same as if the member were opened by its full filename path.
     *
     * Override the standard openUnchecked() to try case-folding the member name to
     * lower-case if the first open attempt returns Empty. Succeed if opening the
     * case-folded name yields a user.
     *
     * @param member the member name within the container
     * @param principal the principal responsible for this operation
     * @param options options which may affect how the file is opened
     * @return a boxed VFile for the member if successful, Empty if the member does
     *         not exist, or Failure on error
     */
    override protected def openUnchecked(member: String, principal: Principal, options: CfsOpenOptions): Box[VFile] = {
        super.openUnchecked(member, principal, options) match {
            case Empty ⇒
                super.openUnchecked(member.toLowerCase, principal, options) match {
                    case Full(uinfo : UserInfo) ⇒ Full(uinfo)
                    case Full(other) ⇒
                        other close ()
                        Empty
                    case _ : EmptyBox ⇒ Empty
                }
            case other ⇒ other
        }
    }

    /**
     * This overrides the standard create() to check additional access rights for adding
     * members to a group. A group may contain only users and other groups, and there are
     * separate access rights for each.
     *
     * @param member the name for the new member, which must not already exist,
     *               unless the filesystem implements versioning
     * @param mimeType the MIME type associated with the new file
     * @param options options that may affect the create
     * @return a boxed VFile if the file is created successfully, or a
     *         Failure otherwise. Empty should not be returned.
     */
    override def create(member : String, mimeType : String, options : CfsCreateOptions) : Box[CfsFile] = {
        val mtid = MimeType findOrCreate mimeType
        if (GroupInfo isType_? mtid) (GroupInfo canAddSubgroup this) { () ⇒
            createUnchecked (member, mimeType, options)
        }
        else if (UserInfo isType_? mtid) (GroupInfo canAddUser this) { () ⇒
            createUnchecked (member, mimeType, options)
        }
        else Failure(s"user group may not contain $mimeType")
    }

    override def linkUnchecked(name : String, member : VFile) : Box[VFile] = {
        member match {
            case uinfo : UserInfo ⇒ GlobalConfig.clearAdminCache(uinfo.getResourceId)
            case _ ⇒ GlobalConfig.clearAdminCache()
        }
        super.linkUnchecked(name, member)
    }

    /**
     * Unlink a member from this container. If this is the last link to the member,
     * it is deleted from the filesystem, and a recursive unlink is performed on its
     * metadata (if metadata is supported). If the member is itself a container, it
     * must be empty (except for metadata), or else the recursive option must be
     * true. A recursive unlink may be partially completed before returning an error.
     *
     * Override the standard unlink() here to implement separate access rights for
     * removing users or groups. Also clear the cache of known administrators, since
     * a user or subgroup may be getting removed from the Admin group.
     *
     * @param member the name of the member to be unlinked from this container
     * @param recursive if the member is a container, recursively unlink its members
     * @return a boxed Boolean which is true if the member was deleted, false if the
     *         link was removed but other links to the member remain. Failure is returned
     *         if the member was not a member of the container, or the principal lacked
     *         the proper access rights, or on any other error.
     */
    override def unlink(member: String, recursive: Boolean): Box[Boolean] = {
        // We don't check canUnlinkObject because the rights we already checked (below)
        // are considered sufficient. Note that removing a group from a non-group
        // container will check GroupInfo.canUnlinkObject in addition to whatever
        // container type-specific rights are required to unlink a member.
        withMember(member) {
            case uinfo : UserInfo ⇒ (GroupInfo canRemoveUser this) (() ⇒ unlinkUnchecked(uinfo, recursive))
            case ginfo : GroupInfo ⇒ (GroupInfo canRemoveSubgroup this) (() ⇒ unlinkUnchecked(ginfo, recursive))
        }
    }

    override def unlinkUnchecked(memberfile : VFile, recursive : Boolean) : Box[Boolean] = {
        memberfile match {
            case uinfo : UserInfo ⇒ GlobalConfig.clearAdminCache(uinfo.getResourceId)
            case _ ⇒ GlobalConfig.clearAdminCache()
        }
        super.unlinkUnchecked(memberfile, recursive)
    }

    def getData : Box[GroupDesc] = (GroupInfo canReadAttributes this) (() ⇒ getDataUnchecked)

    def putData(data : GroupDesc) : Box[Boolean] = canWriteAttributes (this) { () ⇒
        putDataUnchecked (data) map (_ ⇒ true)
    }

    override def asMap : Map[String, Any] = {
        super.asMap ++ (getGroupDesc map ( _.asMap ) openOr Map())
    }

    /**
     * If a given file is a member of this container, get the name it has in this
     * container, which may be different than the name in its file handle path.
     *
     * Override the standard getMemberName() to add access control for groups.
     *
     * @param member the potential member of this container
     * @return the boxed name of the member if it is a member of this container,
     *         Empty if it is not a member, or Failure on error
     */
    override def getMemberName(member: CfsFile): Box[String] = canListMember (this) { () ⇒
        super.getMemberName(member)
    }

    /**
     * Specialized version of the standard contains_?() for users. Both call getMemberName(),
     * which checks access control for groups at least.
     *
     * This only considers whether the user is an immediate member of this group.
     * For some purposes the question is whether a user is a member of this group
     * or any of its descendants. See findUser_!().
     *
     * @param uinfo a UserInfo file handle for a user who might be in this group
     * @return true if the specified is a member of the group
     */
    def isMember_?(uinfo : UserInfo) : Boolean = this contains_? uinfo

    /**
     * This is an optimized version of create() for new users.
     *
     * @param username the username, which will be folded to lower-case for use as a filename,
     *                 but will be saved in its original form in the user settings
     * @param password the password, which may or may not already be encrypted
     * @param email optional e-mail address of the user
     * @param regCode optional registration code for the user
     * @param pwdIsEncrypted true if the password is already encrypted
     * @param oauthOnly true if this user can only be authenticated via oauth, i.e. the user
      *                  has no password
     * @return a boxed user file handle, or Failure
     */
    def addNewUser(username : String, password : String,
                   email : Option[String], regCode : Option[String],
                   pwdIsEncrypted : Boolean = false,
                   oauthOnly : Boolean = false) : Box[UserInfo] = canAddUser (this) { () ⇒
        // Plain text password is passed as "password" option.
        // Already encrypted password is passed as "cryptPwd" option
        val options =
            if (oauthOnly) UserCreateOptions(cryptPwd = Some("ABC"), email = email,
                                             regcode = regCode, oauthOnly = Some(true))
            else if (pwdIsEncrypted) UserCreateOptions(cryptPwd = Some(password), email = email, regcode = regCode)
            else UserCreateOptions(password = Some(password), email = email, regcode = regCode)
        createUnchecked(username, UserInfo.getMimeType, options) match {
            case Full(uinfo : UserInfo) ⇒ Full(uinfo)
            case Full(other) ⇒
                unlink (other.getName, recursive = false)
                other close ()
                Failure(s"$username is not a user")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * This is an optimized version of link() for linking existing users into the group.
     *
     * @param username the username, which will be folded to lower-case for use as a filename
     * @param uinfo the user file handle of the user to be linked into the group, which is
     *              not the same as the returned file handle, and is not closed by this
     *              operation
     * @return a boxed user handle for the user as a member of this group, or Failure
     */
    def addUser(username : String, uinfo : UserInfo) : Box[UserInfo] = canAddUser (this) { () ⇒
        val srcUsername = uinfo.getUsername
        linkUnchecked(username, uinfo) match {
            case Full(myuser : UserInfo) ⇒ Full(myuser)
            case Full(other) ⇒
                unlink (username, recursive = false)
                other close ()
                Failure(s"$srcUsername is not a user")
            case e : EmptyBox ⇒ e
        }
    }

    def copyUsers(tginfo : GroupInfo, users : List[String],
                  rename : Option[Map[String, String]]) : List[(String, Box[UserInfo])] = {
        val mapname = rename getOrElse Map.empty
        val result : List[(String, Box[UserInfo])] = expandUserList (users) map {
            case (suname, Full(suinfo)) ⇒
                val tuname = mapname getOrElse (suname, suname)
                val pair : (String, Box[UserInfo]) = tginfo addUser (tuname, suinfo) match {
                    case Full(tuinfo : UserInfo) ⇒ (tuname, Full(tuinfo))
                    case Full(other) ⇒
                        val oname = other.getName
                        tginfo unlink (oname, recursive = false)
                        other close ()
                        (oname, Failure("not a user"))
                    case e : EmptyBox ⇒ (tuname, e)
                }
                suinfo close ()
                pair
            case (suname, Empty) ⇒ (suname, Failure("should not happen"))
            case (suname, f : Failure) ⇒ (suname, f)
        }
        result
    }

    def expandUserList(users : List[String]) : List[(String, Box[UserInfo])] = {
        def helper(uinfo : UserInfo) : (String, Box[UserInfo]) = {
            (uinfo.getUsername, Full(uinfo))
        }
        users match {
            case "**" :: Nil ⇒ getUsers_! map helper
            case "*" :: Nil ⇒ getUsers map helper
            case list ⇒ list map { uname ⇒
                findUser (uname) match {
                    case Full(uinfo) ⇒ (uname, Full(uinfo))
                    case Empty ⇒ (uname, Failure(s"$uname not found"))
                    case f : Failure ⇒ (uname, f)
                }
            }
        }
    }

    def removeUsers(users : List[String]) : List[(String, Box[Boolean])] = canRemoveUser (this) { () ⇒
        val result : List[(String, Box[Boolean])] = expandUserList (users) map {
            case (uname, Full(uinfo)) ⇒
                val result = unlinkUnchecked(uinfo, recursive = false) match {
                    case Full(b) ⇒ (uname, Full(b))
                    case e : EmptyBox ⇒ (uname, e)
                }
                uinfo close()
                result
            case (uname, Empty) ⇒ (uname, Failure("should not happen"))
            case (uname, f : Failure) ⇒ (uname, f)
        }
        Full(result)
    } openOr Nil

    def validateUser(username : String, password : Option[String]) : Box[UserInfo] = {
        (findUser_! (username) flatMap { uinfo ⇒
            // password == None means ignore the password here
            val ok = password.forall(pwd ⇒ uinfo validPassword_? Some(pwd))
            if (ok) Full(uinfo)
            else {
                uinfo close ()
                Failure("invalid username/password")
            }
        }) ?~ "invalid username/password"
    }

    def setUserPassword(username : String, newPassword : String) : Box[Boolean] = {
        (findUser_! (username) flatMap { uinfo ⇒
            val result = uinfo setPassword newPassword
            uinfo close ()
            result
        }) ?~ s"no such user: $username"
    }
}

/**
 * This is the deserialized representation of the contents of a GroupInfo file.
 */
case class GroupDesc(oldGroupId : Option[Long],
                     description : Option[String],
                     loginEnabled : Option[Boolean],
                     googleLoginEnabled : Option[Boolean],
                     signupEnabled : Option[Boolean],
                     googleSignupEnabled : Option[Boolean],
                     captchaEnabled : Option[Boolean],
                     verifyEmailEnabled : Option[Boolean],
                     regKey : Option[String],
                     guestPage : Option[String],
                     homePage : Option[String]) extends CanHazMap {

    def isLoginEnabled_? : Boolean = loginEnabled getOrElse false
    def isGoogleLoginEnabled_? : Boolean = googleLoginEnabled getOrElse false
    def isSignupEnabled_? : Boolean = signupEnabled getOrElse false
    def isGoogleSignupEnabled_? : Boolean = googleSignupEnabled getOrElse false
    def isCaptchaEnabled_? : Boolean = captchaEnabled getOrElse false
    def isVerifyEmailEnabled_? : Boolean = verifyEmailEnabled getOrElse false

    def setLoginEnabled(enable : Boolean) : GroupDesc = {
        if (loginEnabled contains enable) this else copy(loginEnabled = Some(enable))
    }

    def setGoogleLoginEnabled(enable : Boolean) : GroupDesc = {
        if (googleLoginEnabled contains enable) this else copy(googleLoginEnabled = Some(enable))
    }

    def setSignupEnabled(enable : Boolean) : GroupDesc = {
        if (signupEnabled contains enable) this else copy(signupEnabled = Some(enable))
    }

    def setGoogleSignupEnabled(enable : Boolean) : GroupDesc = {
        if (googleSignupEnabled contains enable) this else copy(googleSignupEnabled = Some(enable))
    }

    def setCaptchaEnabled(enable : Boolean) : GroupDesc = {
        if (captchaEnabled contains enable) this else copy(captchaEnabled = Some(enable))
    }

    def setVerifyEmailEnabled(enable : Boolean) : GroupDesc = {
        if (verifyEmailEnabled contains enable) this else copy(verifyEmailEnabled = Some(enable))
    }

    /**
      * Set a required registration key for the group. If this is set, any self-registration
      * must include the specified key. A group administrator can use this to limit who can
      * self-register by limiting distribution of the key.
      *
      * @param key an optional self-registration key
      * @return an updated GroupDesc
      */
    def setRegKey(key : Option[String]) : GroupDesc = {
        if (regKey == key) this else copy(regKey = key)
    }

    def setGuestPage(page : Option[String]) : GroupDesc = {
        if (guestPage == page) this else copy(guestPage = page)
    }

    def setHomePage(page : Option[String]) : GroupDesc = {
        if (homePage == page) this else copy(homePage = page)
    }

    def setDescription(desc : Option[String]) : GroupDesc = {
        if (description == desc) this else copy(description = desc)
    }

    /**
      * This map should include only information which might be needed by a client for the
      * purpose of customizing a login or signup user experience. It does not include the
      * registration key, as that is checked on the server side during self-registration.
      *
      * The attribute naming may appear somewhat inconsistent. This is to remain backward
      * compatible with the names historically returned by GroupInfo asMap().
      *
      * @return a map of attributes for the group
      */
    override def asMap : Map[String, Any] = {
        Map("desc" → description,
            "login" → isLoginEnabled_?,
            "googleLoginEnabled" → isGoogleLoginEnabled_?,
            "signup" → isSignupEnabled_?,
            "googleSignupEnabled" → isGoogleSignupEnabled_?,
            "captcha" → isCaptchaEnabled_?,
            "verifyEmailEnabled" → isVerifyEmailEnabled_?,
            "regKeyEnabled" → regKey.exists(_ != ""),
            "guest" → guestPage,
            "home" → homePage)
    }
}

class GroupNode(resource : Resource) extends CfsFnode(resource)
    with AtomicDataNode[GroupNode]
    with JsonDataNode[GroupNode, GroupDesc] with IsPrincipal {
    val mf : Manifest[GroupDesc] = manifest[GroupDesc]

    override def getSelfPrincipal = CfsPrincipal(this)

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = true

    /**
     * Create a file handle (VFile or its subclass) for a resource of this MIME type.
     *
     * @param path the filename path used to locate the file
     * @param principal the principal responsible for this open operation
     * @param options options that may affect the open operation
     * @return a boxed VFile (or subclass) for the file associated with the vnode. A Failure
     *         may be returned for various errors, such as insufficient access rights, or
     *         unsupported options.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[GroupInfo] = {
        Full(new GroupInfo (path, principal, this))
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
    override def cfsCreate(name: String, principal: Principal, options: CfsCreateOptions): Box[CfsVnode] = {
        options match {
            case groupOpt : GroupCreateOptions ⇒
                groupOpt.gdesc match {
                    case Some(gdescobj) ⇒
                        putData(gdescobj)
                    case None ⇒ Full(this)
                }
            case _ ⇒ Full(this)
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
     * @return the boxed name to use for the member, which will be case-folded to
     *         lower case, or Failure
     */
    override def acceptName_?(name: String, member : CfsVnode): Box[String] = {
        val namebox = super.acceptName_?(name, member)
        member match {
            case unode : UserNode ⇒
                namebox flatMap { memberName ⇒
                    val lcMemberName = memberName.toLowerCase
                    if (isNameUsed_? (lcMemberName, Some(unode))) {
                        Failure(s"another user '$lcMemberName' already exists in the group hierarchy")
                    }
                    else Full(lcMemberName)
                }
            case _ ⇒ namebox
        }
    }

    /**
     * Check whether a given (user) name is used anywhere in a group hierarchy which
     * contains this GroupNode, and is not associated with a given UserNode. This
     * allows the same user to appear with the same username in multiple levels of
     * the group hierarchy, but ensures that a username will refer to a unique user
     * within any group hierarchy in which the username is present.
     *
     * If unode is None, the user being added is assumed to be a completely new user,
     * so the specified name must not exist in any of this group's hierarchies at all.
     *
     * @param name the username
     * @param unode an optional UserNode for a user proposed to be added to the group
     *              with the specified username
     * @return true if the name is used by any user in the hierarchy other than the
     *         unode user. Otherwise false.
     */
    def isNameUsed_?(name : String, unode : Option[UserNode]) : Boolean = {
        // Given a path to a group, iterate through successive parents in the path
        // until a non-group is found. Return the last group found, which should be the
        // root of a group hierarchy containing the original group.
        @tailrec
        def findRootGroup(path : CfsPath, acc : Option[GroupInfo]) : Option[GroupInfo] = {
            Cfs open (path, BootPrincipal, CfsOpenOptions.Default) match {
                case Full(ginfo : GroupInfo) ⇒
                    // Got a group. Close the previous root candidate, and use this group
                    // as the new candidate.
                    acc foreach (_ close ())
                    findRootGroup (path.getParent, Some(ginfo))
                case Full(other) ⇒
                    // Got a non-group. Close it and return the last root candidate.
                    other close ()
                    acc
                case _ : EmptyBox ⇒ acc     // shouldn't happen
            }
        }
        val newuserId = unode map (_.getResourceId)
        // Determine if the name is in use in any group hierarchy containing this group
        findPaths (BootPrincipal) map { pathlist ⇒
            // For each file path that references this group...
            pathlist exists { path ⇒
                // Find the root of the group hierarchy containing this group
                findRootGroup (path, None) match {
                    case Some(rginfo) ⇒
                        // Does that hierarchy contain the name?
                        val conflict = rginfo findUser_! name match {
                            case Full(uinfo) ⇒
                                // Is it the same user we're trying to add?
                                val sameuser = newuserId contains uinfo.getResourceId
                                uinfo close ()
                                !sameuser
                            case _ : EmptyBox ⇒ false
                        }
                        rginfo close ()
                        conflict
                    case None ⇒ false        // shouldn't happen
                }
            }
        } openOr false
    }
}

object GroupInfo extends ContainerMimeType {

    override protected val Log = Logger("choice.fs.GroupInfo")

    override val getName = "User Group File Type"

    override val getMimeType = "choice/group"

    override val getSchemas : List[BaseMetaMapper] = Nil

    override def isContainer_? = true

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override val getRights : List[RightDef] = List(
        RightDef("read_group_attr", "user group file", "read group attributes"),
        RightDef("list_group", "user group file", "list group members"),
        RightDef("write_group_attr", "user group file", "write group attributes"),
        RightDef("add_user", "user group file", "add user to a group"),
        RightDef("remove_user", "user group file", "remove user from a group"),
        RightDef("add_subgroup", "user group file", "add a subgroup to a group"),
        RightDef("remove_subgroup", "user group file", "remove a subgroup from a group"),
        RightDef("create_group", "user group file", "create a user group"),
        RightDef("unlink_group", "user group file", "unlink a user group")
    )

    def apply(path : String, by : Principal) : Box[GroupInfo] = {
        Cfs.withValidPath(path) { cfspath ⇒ apply(cfspath, by) }
    }

    def apply(path : CfsPath, by : Principal) : Box[GroupInfo] = {
        Cfs open (path, by, CfsOpenOptions.Default) match {
            case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
            case Full(xfile) ⇒
                xfile.close()
                Failure(s"'${path.toString}' is not a user group")
            case e : EmptyBox ⇒ e
        }
    }

    def apply(fileId : CfsVFileId, principal : Principal) : Box[GroupInfo] = {
        Cfs open (fileId, principal, CfsOpenOptions.Default) match {
            case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
            case Full(xfile) ⇒
                val path = xfile.getPath.toString
                xfile.close()
                Failure(s"'$path' is not a user group")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Construct a user group Vnode from a DB Resource object.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    override def instantiate(resource : Resource) : Box[GroupNode] = {
        if (isType_?(resource)) Full(new GroupNode (resource))
        else Failure(s"instantiate: resource id ${resource.getSafeKey.id} is not a user group")
    }

//    def getGroupDesc(oldGroupId : Option[Long],
//                     description : Option[String],
//                     loginEnabled : Boolean = false,
//                     signupEnabled : Boolean = false,
//                     captchaEnabled : Boolean = false) : String = {
//        GroupDesc(oldGroupId, description, Some(loginEnabled), Some(signupEnabled), Some(captchaEnabled)).toString
//    }

    /**
     * This defines the rights needed to list members of the container, or to reference
     * them by name.
     *
     * @return a RightsCheck instance
     */
    val canListMember : RightsCheck = AnyRightsCheck(
        "list_group",
        "add_user", "remove_user",
        "add_subgroup", "remove_subgroup",
        "create_group", "unlink_group"
    )

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    val canAddMember = AllRightsCheck("add_user", "add_subgroup")

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    val canRemoveMember = AllRightsCheck("remove_user", "remove_subgroup")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_group")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_group")

    val canAddUser = AnyRightsCheck("add_user")
    val canRemoveUser = AnyRightsCheck("remove_user")
    val canAddSubgroup = AnyRightsCheck("add_subgroup")
    val canRemoveSubgroup = AnyRightsCheck("remove_subgroup")
    val canReadAttributes = AnyRightsCheck("read_group_attr")
    val canWriteAttributes = AnyRightsCheck("write_group_attr")
    val canCreateGroup = AnyRightsCheck("create_group")
}