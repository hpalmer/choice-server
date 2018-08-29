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
  * Implement the "choice/user" file type, which represents a user.
  *
  * @author Howard Palmer
  */
package choice.fs

import _root_.choice.model._
import _root_.org.jasypt.digest.StandardStringDigester
import choice.access._
import choice.actor.DbManager
import choice.parser.CfsPathParser
import net.liftweb.common._
import net.liftweb.json.DefaultFormats
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.Helpers.millis
import net.liftweb.util.Helpers.tryo

import scala.annotation.tailrec

/**
 * Container object for user settings. This is serialized to JSON and stored as the user
 * file content.
 *
 * @param username optional username without case folding
 * @param cryptPwd encrypted password
 * @param banned if true, this user is not allowed to login
 * @param email optional e-mail address of the user
 * @param regCode optional registration code
 */
case class UserSettings(username : Option[String],
                        cryptPwd : String,
                        banned : Boolean,
                        email : Option[String],
                        regCode : Option[String],
                        oauthOnly : Option[Boolean])

class UserCreateOptions(val username : Option[String], val password : Option[String],
                        val email : Option[String], val regcode : Option[String],
                        val cryptPwd : Option[String], val banned : Boolean, val oauthOnly : Option[Boolean],
                        override val dataSeqnum : Long, override val ctime : Long,
                        override val altid : Long, override val replace : Boolean)
    extends CfsCreateOptions(dataSeqnum = dataSeqnum, ctime = ctime, altid = altid, replace = replace)

object UserCreateOptions {
    def apply(username : Option[String] = None, password : Option[String] = None,
              email : Option[String] = None, regcode : Option[String] = None,
              cryptPwd : Option[String] = None, banned : Boolean = false, oauthOnly : Option[Boolean] = None,
              dataSeqnum : Long = 0L, ctime : Long = millis, altid : Long = 0L, replace : Boolean = false) : UserCreateOptions = {
        new UserCreateOptions(username, password, email, regcode, cryptPwd, banned, oauthOnly,
                              dataSeqnum, ctime, altid, replace)
    }
}

class UserNode(resource : Resource) extends CfsSpecialNode(resource)
        with AtomicDataNode[UserNode]
        with JsonDataNode[UserNode, UserSettings] with IsPrincipal {
    val mf : Manifest[UserSettings] = manifest[UserSettings]

    override def getSelfPrincipal = CfsPrincipal(this)

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

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
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal, options : CfsOpenOptions) : Box[UserInfo] = {
        Full(new UserInfo (path, principal, this))
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
        assert(this.isReferenced_?)
        options match {
            case userOpt : UserCreateOptions ⇒
                val username = userOpt.username getOrElse name
                UserInfo validateUsername name match {
                    case Empty ⇒
                        // Get the encrypted password
                        UserInfo getEncryptedPassword (username, userOpt) flatMap { cryptPwd ⇒
                            val settings = UserSettings(Some(username), cryptPwd, userOpt.banned,
                                userOpt.email, userOpt.regcode, userOpt.oauthOnly)
                            putData(settings)
                        }
                    case f : Failure ⇒ f
                }
            case _ ⇒ Failure(s"missing user creation options for user '$name'")
        }
    }
}

/**
 * This consolidates information about a user from several tables in the database.
 * The companion object generally should be used to construct instances, starting
 * from different sets of information.
 */
class UserInfo(path : CfsAbsolutePath, principal : Principal, vnode : UserNode)
	extends CfsSpecial(path, principal, vnode)
    with JsonDataFile[UserInfo, UserNode, UserSettings] with IsPrincipal {
    implicit val formats : DefaultFormats.type = DefaultFormats

    /**
     * Get the principal represented by this user itself, which is not necessarily
     * the same as the principal which opened the user object.
     *
     * @return a Principal for this user
     */
    def getSelfPrincipal : CfsPrincipal = CfsPrincipal(this)

    def getUsername : String = path.getFileName.toString


    def getData : Box[UserSettings] = (UserInfo canReadSettings this) { () ⇒
        getDataUnchecked
    }

    def putData(data : UserSettings) : Box[Boolean] = (UserInfo canWriteSettings this) { () ⇒
        putDataUnchecked (data) map (_ ⇒ true)
    }

    var _inGroup : Option[GroupInfo] = None

    def getGroupInfo : GroupInfo = _inGroup match {
        case Some(ginfo) ⇒ ginfo
        case None ⇒
            Cfs open (path.getParent, SystemPrincipal, CfsOpenOptions.Default) match {
                case Full(ginfo : GroupInfo) ⇒
                    _inGroup = Some(ginfo)
                    ginfo
                case Full(_) ⇒ sys.error (s"'${path.getParent.toString}' is not a user group")
                case Empty ⇒ sys.error (s"missing user group '${path.getParent.toString}'")
                case f : Failure ⇒ sys.error (f.toString)
            }
    }

    /**
     * Get a list of tuples containing each user group and username for this user.
     * A user can belong to multiple user groups, under the same or different
     * usernames. A user could belong to one group under multiple usernames.
     */
    def getGroupList : List[(String, GroupInfo)] = {
        @tailrec
        def helper(list : List[(CfsVnode, String)], acc : List[(String, GroupInfo)]) : List[(String, GroupInfo)] = {
            list match {
                case Nil ⇒ acc
                case head :: tail ⇒
                    val (vnode, name) = head
                    val pginfo = Cfs open (vnode.getFileId, principal, CfsOpenOptions.Default /* "traverse" -> true ? */)
                    vnode.release
                    pginfo match {
                        case Full(ginfo : GroupInfo) ⇒
                            // Verify that the user can see their entry in this group
                            ginfo getMember name match {
                                case Full(uinfo) ⇒
                                    uinfo close ()
                                    helper(tail, (name, ginfo) :: acc)
                                case _ : EmptyBox ⇒ helper(tail, acc)
                            }
                        case Full(other) ⇒
                            // Users are only contained by groups, so this shouldn't happen
                            other close ()
                            helper(tail, acc)
                        case _ : EmptyBox ⇒ helper(tail, acc)
                    }
            }
        }
        DbManager findContainers vnode match {
            case Full(list) ⇒ helper (list, Nil)
            case _ : EmptyBox ⇒ Nil
        }
    }
    
    def isLoginAllowed_? : Boolean = (UserInfo canReadAttributes this) { () ⇒
        getDataUnchecked map (!_.banned)
    } openOr false

    def isOAuthOnly_? : Boolean = (UserInfo canReadAttributes this) { () ⇒
        getDataUnchecked map (_.oauthOnly.getOrElse(false))
    } openOr false

    def validPassword_?(password : Option[String]) : Boolean = (UserInfo canReadAttributes this) { () ⇒
        getDataUnchecked flatMap { settings ⇒
            val result =
                if (settings.oauthOnly.getOrElse(false)) false
                else UserInfo checkPassword (password, settings.cryptPwd)
            Full(result)
        }
    } openOr false

    def setPassword(newPassword : String) : Box[Boolean] = (UserInfo canWriteAttributes this) { () ⇒
        (UserInfo validatePassword newPassword flatMap { validpwd ⇒
            val cryptPwd = UserInfo.getDigester digest validpwd
            getDataUnchecked flatMap { settings ⇒
                putData (settings copy (cryptPwd = cryptPwd))
            }
        }) ?~ s"new password validation failed for $getUsername"
    }

    def banUser(ban : Boolean) : Box[Boolean] = (UserInfo canWriteAttributes this) { () ⇒
        getDataUnchecked flatMap { settings ⇒
            if (settings.banned == ban) Full(ban)
            else {
                putData (settings copy (banned = ban)) map (_ ⇒ ban)
            }
        }
    }

    override def getVnode : UserNode = vnode

    /**
     * Close the handle. This will also close any open input or output streams,
     * and release any other resources associated with the VFile.
     */
    override def close() : Unit = {
        _inGroup foreach (_ close ())
        super.close()
    }

    override def asMap : Map[String, Any] = {
        (UserInfo canReadAttributes this) { () ⇒
            getDataUnchecked flatMap { settings ⇒
                val base = Map[String, Any]("isLoginAllowed" → !settings.banned,
                                "regcode" → settings.regCode,
                                "oauthOnly" → settings.oauthOnly)
                (UserInfo canReadIdentity this) { () ⇒
                    val ident = base ++ Map("username" → (settings.username getOrElse getUsername),
                                            "email" → settings.email)
                    Full(ident)
                } match {
                    case Full(idmap) ⇒ Full(idmap)
                    case _ : EmptyBox ⇒ Full(base)
                }
            }
        } match {
            case Full(umap) ⇒ umap ++ super.asMap
            case _ : EmptyBox ⇒ super.asMap
        }
    }
}

object UserInfo extends MimeTypeHandler {

    override protected val Log = Logger("choice.fs.UserInfo")

    override val getName = "User File Type"

    override val getSchemas : List[BaseMetaMapper] = Nil

    /**
     * Return the list of access rights for this MIME type.
     *
     * @return a list of all the access rights for files of this MIME type
     */
    override val getRights : List[RightDef] = List(
        RightDef("create_user", "user file", "create a user"),
        RightDef("unlink_user", "user file", "unlink a user"),
        RightDef("get_user_id", "user file", "get numeric user id, but not username or email"),
        RightDef("get_user_ident", "user file", "read username and email address"),
        RightDef("set_user_ident", "user file", "set username and email address"),
        RightDef("get_user_attr", "user file", "get all user attributes except username and email"),
        RightDef("set_user_attr", "user file", "set all user attributes except username and email"),
        RightDef("list_user_meta", "user file", "list metadata attribute names"),
        RightDef("add_user_meta", "user file", "create a new metadata entry")
    )

    def apply(path : CfsPath, by : Principal) : Box[UserInfo] = {
        Cfs open (path, by, CfsOpenOptions.Default) match {
            case Full(uinfo : UserInfo) ⇒ Full(uinfo)
            case Full(_) ⇒ Failure(s"'${path.toString}' is not a user")
            case e : EmptyBox ⇒ e
        }
    }
            
    override val getMimeType : String = "choice/user"

    override def instantiate(resource : Resource) : Box[UserNode] = {
        if (isType_? (resource)) Full(new UserNode(resource))
        else Failure(s"UserInfo instantiate passed a non-user resource")
    }

    def invalidate(resid : ResourceId, mtid : MimeTypeId) : Unit = {
        //UserManager.invalidateUser(resid, mtid)
    }

    def validateUsername(username : String) : EmptyBox = {
//        username match {
//            case u if u.length < 3 ⇒ Failure("username must contain at least 3 characters")
//            case u if u.length > 32 ⇒ Failure("username must not be longer than 32 characters")
//            case u if !Character.isLetter(u(0)) ⇒ Failure("username must start with a letter")
//            case u if u exists (!_.isLetterOrDigit) ⇒ Failure("username must contain only letters or digits")
//            case u ⇒ Empty
//        }
        val parts = username.split('@')
        val namebox = parts.length match {
            case n if n < 1 ⇒ Failure("username must contain at least 3 characters")
            case n if n == 1 ⇒
                if (username.length < 3) Failure("username must contain at least 3 characters")
                else Full(username)
            case n if n == 2 ⇒
                val dparts = parts(1).split('.')
                if (username.length < 256 && dparts.forall { d ⇒
                    !d.startsWith("-") && !d.endsWith("-") &&
                    d.length > 0 && d.length < 64 &&
                    d.forall(c ⇒ c.isLetterOrDigit || c == '-')
                }) {
                    Full(username)
                }
                else Failure("username is an acceptable email address")
            case _ ⇒ Failure("username is not an acceptable email address")
        }
        namebox match {
            case Full(uname) ⇒
                CfsPathParser.filename(uname) match {
                    case Full(_) ⇒ Empty
                    case e : EmptyBox ⇒ e
                }
            case e : EmptyBox ⇒ e
        }
    }

    def getEncryptedPassword(username : String, options : UserCreateOptions) : Box[String] = {
        // Allow the encrypted password to be specified directly. This is needed
        // for migration, where the unencrypted password is unknown.
        options.cryptPwd match {
            case Some(cpwd) ⇒ Full(cpwd)
            case None ⇒
                // Otherwise try for a plain text password, and encrypt it if a valid
                // one is present.
                options.password match {
                    case Some(plainpwd) ⇒
                        validatePassword(plainpwd) match {
                            case Full(validPwd) ⇒ Full(UserInfo.getDigester digest validPwd)
                            case e : EmptyBox ⇒ e
                        }
                    case None ⇒ Failure(s"missing a password for user '$username'")
                }
        }
    }

    // Generated password length
    val GENPWDLENGTH = 5

    /**
     * Generate a random password. The length of the generated password currently
     * defaults to 5 characters, but may be overridden.
     *
     * @return the generated password string
     */
    def generatePassword(length : Int = 5) : String = {
        import java.security.SecureRandom
        val rnd = new SecureRandom
        val pwd = new StringBuilder(length)
        for (_ ← 0 until length) {
            pwd += passwordChars.charAt(rnd.nextInt(passwordChars.length))
        }
        pwd.toString()
    }

    // Characters in generated passwords (digit 1 and letter l omitted)
    val passwordChars = "abcdefghijkmnopqrstuvwxyz023456789"
    val validPasswordSpecialChars = "~!@#$%^&*()-_+=[]{}/\\\"<>,.?;:'"

    /**
     * Validate a password for a new user account. Passwords are always
     * case-folded.
     * 
     * @param password  a password string
     * @return a boxed password string or Failure
     */
    def validatePassword(password : String) : Box[String] = {
        val p = password.toLowerCase
        // Check for a legal password
        p.toSeq.find(c ⇒ !(c.isLetterOrDigit || validPasswordSpecialChars.contains(c))) match {
            case Some(c) ⇒ Failure("character not allowed in password: '" + c + "'")
            case None ⇒ Full(p)
        }
    }
    
    /**
     * Check that a specified password matches a hashed password. Leading and
     * trailing whitespace is stripped and case is folded prior to hashing
     * the specified password.
     * 
     * @param password  the password to be checked, default to empty string
     * @param hash      the hash value it should match
     * 
     * @return true if the password matches, false otherwise
     */
    def checkPassword(password : Option[String], hash : String) : Boolean = {
        val cleanpwd = password.getOrElse("").trim.toLowerCase
        tryo(getDigester.matches(cleanpwd, hash)) openOr false
    }
    
    def getDigester : StandardStringDigester = {
        val sd = new StandardStringDigester
        sd.setAlgorithm("SHA-256")
        sd.setSaltSizeBytes(16)
        sd.setIterations(10)
        sd.initialize()
        sd
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_user")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * This should be the same as canRemoveUser in GroupInfo, so that it is the
     * only right required to remove a user from a group.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("remove_user")

    val canReadAttributes = AnyRightsCheck("get_user_attr")
    val canWriteAttributes = AnyRightsCheck("set_user_attr")
    val canReadIdentity = AnyRightsCheck("get_user_ident")
    val canWriteIdentity = AnyRightsCheck("set_user_ident")
    val canReadSettings = AllRightsCheck("get_user_attr", "get_user_ident")
    val canWriteSettings = AllRightsCheck("set_user_attr", "set_user_ident")
}
