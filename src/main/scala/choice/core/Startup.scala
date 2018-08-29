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
  * Server startup code, launched from the Lift bootstrap code.
  *
  * @author Howard Palmer
  */
package choice.core

import java.net.URI

import choice.access._
import choice.actor.{DbManager, SessionManager}
import choice.attributes.{AttrDef, AttrDefCreateOptions}
import choice.fs._
import choice.fs.archive.CfsJarFile
import choice.fs.vfs.AttributeType
import choice.lib._
import choice.model._
import choice.model.blackbox.BlackBox
import net.liftweb.common._
import net.liftweb.db.DB
import net.liftweb.http.{LiftRules, Req}
import net.liftweb.json.{DefaultFormats, Serialization}
import net.liftweb.mapper.{BaseMetaMapper, Schemifier}
import net.liftweb.util.Helpers._
import net.liftweb.util.{DefaultConnectionIdentifier, Props}

/**
 * This object contains server initialization code. This includes initializing modules,
 * and creating any standard system objects that do not already exist. This includes
 * the file system root folder, the /System folder, the /System/Users and
 * /System/Users/Admin user groups, and the System user. It also includes the
 * /System/Rights and /System/Roles folders and the default rights and roles objects.
 *
 * Someday perhaps the server will support a scripting language, in which case much
 * of this code may be moved to a startup script.
 */
object Startup {

    val Log = Logger("choice.core.Startup")

    /**
      * Property name for the maximum number of VNodes that DbManager will cache.
      * Default is unlimited (except by available memory).
      */
    val VNodeCacheLimit = "choice.VNodeCacheLimit"

    /**
      * Property name for the maximum number of FsName entries that DbManager will cache.
      * Default is unlimited (except by available memory).
      */
    val FsNameCacheLimit = "choice.FsNameCacheLimit"

    val SystemFolderPath = "/System"

    /**
      * The /System/Pending folder contains JSON files which describe a script invocation
      * that will occur in response to a future request. The initial motivation for this
      * is to support sending a link an email, which will cause a script to run when the
      * user clicks it.
      */
    val PendingFolderPath = "/System/Pending"

    /**
      * A dedicated pending folder for password resets.
      */
    val PendingResetPasswordFolderPath = s"$PendingFolderPath/ResetPassword"

    /**
      * Files representing access control rights reside in this folder.
      */
    val RightsFolderPath = "/System/Rights"

    /**
      * Files defining access control roles reside in this folder. Roles are essentially
      * lists of rights.
      */
    val RolesFolderPath = "/System/Roles"

    /**
      * Files representing system-wide access control policies reside in this folder.
      * Access control policies also may be created elsewhere for particular purposes.
      */
    val PoliciesFolderPath = "/System/Policies"

    /**
      * Files representing file metadata attribute definitions reside in this folder.
      */
    val AttributesFolderPath = "/System/Attributes"

    /**
      * Files which record information about user sessions reside in this folder.
      */
    val SessionFolderPath = "/System/Sessions"

    /**
      * This is the default user group for the system. User groups also may be created
      * elsewhere.
      */
    val DefaultUsersPath = "/System/Users"

    /**
      * This is the system administrators user group. Any users who are members of this
      * group have system administrator rights.
      */
    val AdminUsersPath = "/System/Users/Admin"

    /**
      * This is a special administrator user which is used to perform internal
      * administrative tasks. This user never logs in.
      */
    val SystemUserPath = "/System/Users/Admin/system"

    /**
      * This is a special user which represents all users who are not logged in.
      */
    val GuestUserPath = "/System/Users/guest"

    /**
      * This is a special user group to which all logged-in users implicitly belong.
      */
    val AnyoneGroupPath = "/System/Users/anyone"

    /**
      * This is the default home page to which users are directed when no other home
      * page is specified.
      */
    val DefaultHomePage = "group_default.html"

    /**
      * This folder contains HTML files to be sent for various HTTP error status codes.
      */
    val TemplateFolderPath = "/System/Templates"

    /**
      * This is the system mailer object, which is used to send email. Mailer objects
      * also may be created elsewhere.
      */
    val SystemMailerPath = "/System/mailer"

    /**
      * A list of files to which anonymous read access should be allowed.
      * A resource-based access control policy is attached to each file.
      */
    val anonReadFiles = List(
        "/",
        "boot.html",
        "filebrowser.html",
        "group_default.html",
        "groupmgmt.html",
        "index.html",
        "lightsout.html",
        "blackbox.html"
    )

    /**
      * A list of files prefixes to which a path-based access control policy
      * is applied, allowing anonymous read access to all paths starting with
      * these prefixes.
      */
    val anonReadPrefixes = List(
        "/css",
        "/fsdesktop",
        "/images",
        "/js",
        "/library",
        "/resize",
        "/scripts",
        "/target"
    )

    /**
     * DB tables not currently associated with a Module.
     */
    val orphanSchemas : List[BaseMetaMapper] = List(GroupPage, // WebSession, LoginSession,
        PageDef, PageHit)

    /**
     * All modules. Those modules which are MimeTypeHandlers will register their
     * MIME type string with DbManager.
     */
    val modules = List(MimeTypeHandler, ExecutableMimeType, Cfs, CfsFile, Policy, RightFile, RoleFile, CfsJarFile,
        GroupInfo, UserInfo, CfsFolder, CfsPlain, Component, SessionFolder, AttrDef, CfsLuaScript, CfsJsScript,
        CfsMailer)

    /**
     * All DB tables.
     */
    val allSchemas : List[BaseMetaMapper] = (modules foldLeft orphanSchemas) { (list, module) ⇒
        list ++ module.getSchemas
    }

    /**
     * The name of the cookie used for the JSESSIONID.
     */
    var jSessionIdName = ""

    /**
      * This is a template describing an access control role. A role is a named list of access
      * control rights.
      *
      * @param name the role name
      * @param description a description of the role
      * @param rights the role's list of rights
      */
    private case class RoleTemplate(name : String, description : String, rights : List[String]) {
        /**
          * Create the role represented by this template if it does not already exist.
          *
          * @return a boxed RoleFile for the role
          */
        def create() : Box[RoleFile] = {
            RoleFile(name, description, SystemPrincipal) match {
                case Full(role) ⇒
                    rights foreach (role addRight (_, SystemPrincipal))
                    Full(role)
                case e : EmptyBox ⇒
                    Log.error(s"""error creating role $name: ${(e ?~ "Empty").msg}""")
                    e
            }
        }
    }

    /**
      * This is a template describing an access control policy. A policy is essentially
      * a list of associations between principals and roles.
      *
      * @param name the policy name (implicitly in the folder indicated by PoliciesFolderPath)
      * @param roles a list of tuples, each containing a path to a principal (user or group),
      *              followed by the name of a role
      */
    private case class PolicyTemplate(name : String, roles : List[(String, String)]) {
        /**
          * Create the policy represented by this template if it does not already exist.
          *
          * @return a boxed Policy for the policy
          */
        def create() : Box[Policy] = {
            Cfs.withValidPath(s"$PoliciesFolderPath/$name") { rapath ⇒
                Cfs open (rapath, SystemPrincipal, CfsOpenOptions.Default) match {
                    case Full(pfile : Policy) ⇒ Full(pfile)
                    case Full(other) ⇒
                        other close ()
                        Failure(s"$name is not a policy")
                    case Empty ⇒
                        val roleAssignments = roles flatMap {
                            case (principalName, roleName) ⇒
                                // The principal can be a user or a group
                                val pidbox = Cfs.withExistingFile(principalName, SystemPrincipal, CfsOpenOptions.Default) {
                                    case uinfo : UserInfo ⇒ Full(uinfo.getSelfPrincipalId)
                                    case ginfo : GroupInfo ⇒ Full(ginfo.getSelfPrincipalId)
                                }
                                pidbox flatMap { pid ⇒ RoleFile.getRoleId(roleName) map ((pid, _)) } match {
                                    case Full(pair) ⇒ List(pair)
                                    case _ : EmptyBox ⇒
                                        Log.error(s"Problem with ($principalName, $roleName) in $name policy")
                                        Nil
                                }
                        }
                        if (roleAssignments.nonEmpty) {
                            // Create the policy and add its role assignments
                            Policy createPolicy(rapath, SystemPrincipal) flatMap { pfile ⇒
                                pfile addRole roleAssignments
                                Full(pfile)
                            }
                        }
                        else {
                            Failure(s"no valid role assignments for system policy $name")
                        }
                    case f : Failure ⇒ f
                }
            }
        }
    }

    /**
      * Templates for access control roles to be created at startup.
      */
    private val initialRoles : List[RoleTemplate] = List(
        RoleTemplate("folder_traverse", "just traverse folders", List("list_folder")),
        RoleTemplate("runscript_self", "run a script as self", List("run_script")),
        RoleTemplate("runscript_owner", "run a script as owner", List("run_as_owner")),
        RoleTemplate("anonymous", "an anonymous user", List(
            "read_file",
            "get_file_info",
            "list_folder",
            "read_component"
        )),
        RoleTemplate("researcher", "a primary researcher", List(
            "list_folder",
            "read_component",
            "list_session",
            "read_session",
            "get_user_id",
            "get_user_ident",
            "get_user_attr",
            "read_file",
            "get_file_info",
            "run_script",
            "list_group",
            "read_group_attr",
            "read_file_attributes"
        ))
    )

    /**
      * Templates for access control policies to be created at startup.
      */
    private val initialPolicies : List[PolicyTemplate] = List(
        PolicyTemplate("traverse_anyone", List((AnyoneGroupPath, "folder_traverse"))),
        PolicyTemplate("traverse_anonymous", List(
            (GuestUserPath, "folder_traverse"), (AnyoneGroupPath, "folder_traverse")
        )),
        PolicyTemplate("read_anyone", List((AnyoneGroupPath, "anonymous"))),
        PolicyTemplate("read_anonymous", List(
            (GuestUserPath, "anonymous"), (AnyoneGroupPath, "anonymous")
        )),
        PolicyTemplate("runself_anyone", List((AnyoneGroupPath, "runscript_self"))),
        PolicyTemplate("runself_anonymous", List(
            (GuestUserPath, "runscript_self"), (AnyoneGroupPath, "runscript_self")
        )),
        PolicyTemplate("runowner_anyone", List((AnyoneGroupPath, "runscript_owner"))),
        PolicyTemplate("runowner_anonymous", List(
            (GuestUserPath, "runscript_owner"), (AnyoneGroupPath, "runscript_owner")
        ))
    )

    /**
      * The URL used to access the FSChoice database, without the query string.
      */
    lazy val dbUrl : String = {
        DB.use(DefaultConnectionIdentifier) { conn ⇒
            val url = conn.getMetaData.getURL
            val uri = new URI(url)
            val parts = uri.getSchemeSpecificPart.split('?')
            parts(0)
        }
//        val ctx = new InitialContext()
//        val name = net.liftweb.util.DefaultConnectionIdentifier.jndiName
//        try {
//            // ctx.lookup returns a Proxy object, which apparently can provide a getUrl() method,
//            // at least when running under Tomcat 7. Casting the return value to a Tomcat
//            // DataSource (which apparently is what the Proxy is proxying) fails. But using
//            // runtime reflection to access the method seems to work.
//            val tcds = ctx.lookup(s"java:comp/env/$name")
//            val getUrl = tcds.getClass.getMethod("getUrl")
//            val url = getUrl.invoke(tcds).asInstanceOf[String]
//            val uri = new URI(url)
//            val parts = uri.getSchemeSpecificPart.split('?')
//            parts(0)
//        }
//        catch  {
//            case _ : Throwable ⇒
//                throw new RuntimeException(s"unable to get a Tomcat DataSource for $name")
//        }
//        finally {
//            ctx close ()
//        }
    }

    /**
      * Continue booting the system. This is called from the Lift bootstrap.
      *
      * @param jsname the name of the cookie used to track sessions
      */
    def boot(jsname : String) : Unit = {
        jSessionIdName = jsname
        Log.info(s"Using JSESSIONID name: $jsname")

        // Create or update DB schema
        Schemifier schemify (true, Schemifier infoF _, allSchemas : _*)

        LiftRules.calculateContextPath = () ⇒ RequestContextPath.get
        val fixhref = Req.normalizeHref
        Req.normalizeHref = (contextPath, v, fixURL, rewrite) ⇒ {
            val cpath = RequestContextPath.get map { rcp ⇒
                if (rcp == "/") contextPath
                else contextPath + rcp
            }
            fixhref(cpath openOr contextPath, v, fixURL, rewrite)
        }

        // Initialize DbManager. This assigns numeric ids to the MIME type strings
        // previously registered by MimeTypeHandler modules.
        DbManager.initialize()

        // The first Resource created is for the system user. It should have a ResourceId of 1L
        // to match the id of the BootPrincipal.
        val sysuresid = createSystemUserResource.getSafeKey

        // Set a limit on the maximum number of VNodes that will be cached
        Props.get(VNodeCacheLimit) foreach { s ⇒
            try {
                val limit = s.toInt
                DbManager setVNodeCacheLimit limit match {
                    case Full(used) ⇒ Log.info(s"$VNodeCacheLimit set to $limit, $used entries in use")
                    case e : EmptyBox ⇒ Log.error("setVNodeCacheLimit error", e ?~ "Empty")
                }
            }
            catch {
                case _ : NumberFormatException ⇒ Log.error(s"bad value $s for $VNodeCacheLimit")
            }
        }

        // Set a limit on the maximum number of FsName entries that will be cached
        Props.get(FsNameCacheLimit) foreach { s ⇒
            try {
                val limit = s.toInt
                DbManager setFsNameCacheLimit limit match {
                    case Full(used) ⇒ Log.info(s"$FsNameCacheLimit set to $limit, $used entries in use")
                    case e : EmptyBox ⇒ Log.error("setFsNameCacheLimit error", e ?~ "Empty")
                }
            }
            catch {
                case _ : NumberFormatException ⇒ Log.error(s"bad value $s for $FsNameCacheLimit")
            }
        }

        // This will create the filesystem root if it doesn't already exist.
        //
        // The BootPrincipal is intended to serve as the SystemPrincipal, possibly before the
        // "system" user has been created. The Resource for the "system" user is always the first
        // one created in a pristine database, and thus should have a resource id of 1.
        Cfs open (CfsRootPath, BootPrincipal, CfsOpenOptions.Default) foreach {
            case root : CfsFolder ⇒
                Log.info("root folder opened")
                root close ()
                openOrCreateFolder(SystemFolderPath) close ()
                // Create queued scripts folders
                val pendingFolder = openOrCreateFolder(PendingFolderPath)
                // Create the "run" folder, where queued scripts are moved during their execution.
                cleanRunFolder(pendingFolder)
                pendingFolder close ()
                val resetPasswordFolder = openOrCreateFolder(PendingResetPasswordFolderPath)
                cleanRunFolder(resetPasswordFolder)
                resetPasswordFolder close ()
                openOrCreateFolder(RightsFolderPath) close ()
                openOrCreateFolder(RolesFolderPath) close ()
                openOrCreateFolder(PoliciesFolderPath) close ()
                openOrCreateFolder(AttributesFolderPath) close ()
                openOrCreateFolder(SessionFolderPath) close ()
                val defUsers = openOrCreateGroup(DefaultUsersPath)
                openOrCreateGroup(AnyoneGroupPath) close ()
                val admin = openOrCreateGroup(AdminUsersPath)
                // Now that the Admin user group is known to exist, see if the "system" user is in it.
                admin.withMember("system") {
                    case uinfo : UserInfo ⇒ Full(uinfo.getResourceId.id == 1L)
                } match {
                    case Full(b) ⇒ assert(b)
                    case Empty ⇒
                        // The "system" user is not there, so try to link the Resource
                        // we previously created for the "system" user as "system" in the Admin group.
                        val advnode = admin.getVnode
                        DbManager lookupById CfsVFileId(sysuresid) map { sysunode ⇒
                            advnode.withWriteLock { () ⇒
                                sysunode.withWriteLock { () ⇒
                                    // This will fail if the "system" user already exists.
                                    DbManager link (advnode, sysunode, "system") match {
                                        case _ : EmptyBox ⇒
                                            // The reference count of the sysunode Resource anticipated the
                                            // success of this link operation, but it failed. So decrement
                                            // that count to zero. Then releasing sysunode should delete
                                            // Resource and its associated Datanode and ChoiceData file.
                                            sysunode.getResource.removeReference()
                                        case Full(_) ⇒
                                            // The link worked, so this must be a pristine database
                                            assert(sysunode.getResourceId.id == 1L)
                                    }
                                    sysunode.release
                                }
                            }
                        } openOr sys.error(s"failed to link system user in $AdminUsersPath")
                    case f : Failure ⇒ sys.error(s"error looking up system user: ${f.msg}")
                }
                admin close ()
                defUsers.withMember ("guest") {
                    case _ : UserInfo ⇒ Full(true)
                } match {
                    case Full(_) ⇒
                    case Empty ⇒
                        val options = UserCreateOptions(banned = true, password = Some(""))
                        defUsers create ("guest", UserInfo.getMimeType, options) match {
                            case Full(uinfo) ⇒
                                uinfo close ()
                            case _ : EmptyBox ⇒ sys.error(s"failed to create guest user")
                        }
                    case f : Failure ⇒ sys.error(s"error opening $GuestUserPath: ${f.msg}")
                }
                defUsers close ()
        }

        // Create the default Component if necessary
        val defcomp = Component.getDefaultComponent openOr sys.error("failed to get default component")
        defcomp close ()

        // Initialize each module. This creates the access rights associated with each module.
        modules foreach (_.init())

        // Create the initial access control roles from templates describing them
        initialRoles foreach { roleTemplate ⇒
            roleTemplate.create() match {
                case Full(role) ⇒
                    role close ()
                    Log.info(s"created role ${roleTemplate.name}")
                case _ ⇒
                    Log.error(s"failed to create role ${roleTemplate.name}")
            }
        }

        // Create the initial access control policies from templates describing them
        initialPolicies foreach { policyTemplate ⇒
            policyTemplate.create() match {
                case Full(pfile) ⇒
                    pfile close ()
                    Log.info(s"created system policy ${policyTemplate.name}")
                case _ ⇒
                    Log.error(s"failed to create system policy ${policyTemplate.name}")
            }
        }

        // Get the traverse_anyone policy
        val anyoneTraverse =
            Cfs.withExistingFile(s"$PoliciesFolderPath/traverse_anyone", SystemPrincipal, CfsOpenOptions.Default) {
                case pfile : Policy ⇒ Full(pfile.getResourceId)
            } openOr sys.error(s"failed to create traverse_anyone policy")

        // Some additional functionality may be provided through Lua scripts residing in /lua.
        // Allow any logged in user to at least list that folder. Actually executing or reading
        // a script will take additional rights.
        PathPolicy set (anyoneTraverse, "/lua", PathPatternType.PREFIX, None, SystemPrincipal)

        // Get the runowner_anyone policy
        val runownerAnyone =
            Cfs.withExistingFile(s"$PoliciesFolderPath/runowner_anyone", SystemPrincipal, CfsOpenOptions.Default) {
                case pfile : Policy ⇒ Full(pfile.getResourceId)
            } openOr sys.error(s"failed to create runowner_anyone policy")

        // This is to support server-side scripting. Ideally this policy would be applied
        // using resource-based access control. But the file may not exist yet.
        PathPolicy set (runownerAnyone, "/lua/getpage.lua", PathPatternType.PREFIX, None, SystemPrincipal)

        // LiveGroupSpec is an attribute that may be attached to a user group. It contains a JSON-encoded
        // specification of parameters for realtime monitoring of a Choicelet.
        createSystemAttribute("LiveGroupSpec", AttributeType.JSON, Some("realtime data collection specification"))

        // UserScriptFile is an attribute that may be attached to a user group
        createSystemAttribute("UserScriptFile", AttributeType.FILE, Some("server-side script for a user group"))

        // UserScriptIndex is an attribute that may be attached to a user
        createSystemAttribute("UserScriptIndex", AttributeType.NUMBER, Some("user's line index in server-side script"))

        ResponseTemplates.createDefaultTemplates()

        Cfs.withValidPath(SystemMailerPath) { path ⇒
            Cfs.ifNoFile(path, SystemPrincipal) { path ⇒
                val smProps = Props.props.filterKeys(k ⇒ k.startsWith("mailer.system."))
                // Temporary
                val mailerOptions = new MailerCreateOptions(
                    MailerSettings(
                        smProps.getOrElse("mailer.system.host", "localhost"),
                        Some(smProps.getOrElse("mailer.system.port", "25").toInt),
                        smProps.getOrElse("mailer.system.auth", "false").toBoolean,
                        smProps.getOrElse("mailer.system.starttls", "false").toBoolean,
                        Option(smProps.getOrElse("mailer.system.username", null)),
                        Option(smProps.getOrElse("mailer.system.password", null))), 0L, millis, 0L, true)
                Cfs.create(path, SystemPrincipal, "choice/mailer", mailerOptions) match {
                    case Full(mailer : CfsMailer) ⇒
                        mailer close()
                        Log.info("/System/mailer created successfully")
                        Full(true)
                    case Full(other) ⇒
                        other close()
                        val f = Failure(s"/System/mailer is not choice/mailer")
                        Log.error(f.messageChain)
                        f
                    case e : EmptyBox ⇒
                        val f = e ?~ "Empty"
                        Log.error(f)
                        f
                }
            }
        }

        UserOps.init()
        GroupOps.init()
        EventOps.init()
        FileOps.init()
        SessionOps.init()
        BlackBox.init()
        SessionManager.init()

        // Get the read_anonymous policy
        Cfs.withExistingFile(s"$PoliciesFolderPath/read_anonymous", SystemPrincipal, CfsOpenOptions.Default) {
            case anonRead : Policy ⇒
                // Apply the read_anonymous policy to many files
                anonReadFiles foreach (anonRead.protect (_, SystemPrincipal))

                anonReadPrefixes foreach { prefix ⇒
                    PathPolicy set (anonRead.getResourceId, prefix, PathPatternType.PREFIX, None, SystemPrincipal)
                }
                Full(true)
        } openOr sys.error(s"failed to create read_anonymous policy")

        cleanGroupPages()

        DbManager.validateCache
    }

    def createSystemUserResource : Resource = {
        implicit val formats : DefaultFormats.type = DefaultFormats

        // Check whether a resource for the system user already exists
        Resource findByKey 1L openOr {
            // Create a settings JSON file for the system user. The system user is never
            // allowed to login, so the password is moot.
            val settings = UserSettings(Some("System"), "", banned = true, None, None, None)
            val s = Serialization.write(settings)
            val (dataseqnum, _) = ChoiceData makeDataFile s.getBytes
            // Make a UserInfo resource
            Resource storeResource (1L, UserInfo.getMimeTypeId, dataseqnum, millis, 0L) flatMap { res ⇒
                if (res.getOwnerId == res.getSafeKey) Full(res)
                else {
                    res.remove()
                    sys.error("system user resource created with id != 1")
                }
            } openOr sys.error("failed to create system user resource")
        }
    }

    def openOrCreateFolder(path : String, by : Principal = BootPrincipal) : CfsFolder = {
        Cfs.withValidPath (path) { cfspath ⇒
            Cfs open (cfspath, by, CfsOpenOptions.Default) match {
                case Full(folder : CfsFolder) ⇒ Full(folder)
                case Full(_) ⇒ sys.error(s"$path is not a folder")
                case Empty ⇒
                    Cfs create (cfspath, by, CfsFolder.getMimeType) match {
                        case Full(folder : CfsFolder) ⇒ Full(folder)
                        case Full(_) ⇒ sys.error(s"creation of $path did not return a folder")
                        case Empty ⇒ sys.error(s"creation of $path returned Empty")
                        case f : Failure ⇒ sys.error(s"creation of $path failed: ${f.messageChain}")
                    }
                case f : Failure ⇒ sys.error(s"open of $path failed: ${f.messageChain}")
            }
        } openOr sys.error(s"invalid Cfs path: $path")
    }

    def openOrCreateGroup(path : String) : GroupInfo = {
        Cfs.withValidPath (path) { cfspath ⇒
            Cfs open (cfspath, BootPrincipal, CfsOpenOptions.Default) match {
                case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
                case Full(other) ⇒
                    other close ()
                    Failure(s"$path is not a group")
                case Empty ⇒
                    val gdesc = GroupDesc(None, Some("default user group"), None, None, None, None, None, None,
                        None, None, None)
                    val options = GroupCreateOptions(gdesc = Some(gdesc))
                    Cfs.create (cfspath, BootPrincipal, GroupInfo.getMimeType, options) match {
                        case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
                        case Full(_) ⇒ Failure(s"creation of $path did not return a group")
                        case Empty ⇒ Failure(s"creation of $path returned Empty")
                        case f : Failure ⇒ Failure(s"creation of $path failed: ${f.messageChain}")
                    }
                case f : Failure ⇒ Failure(s"open of $path failed: ${f.messageChain}")
            }
        } match {
            case Full(ginfo) ⇒ ginfo
            case e : EmptyBox ⇒ sys.error((e ?~ "Empty").msg)
        }
    }

    def cleanRunFolder(queueFolder : CfsFolder) : Unit = {
        // Create the "run" folder, where queued scripts are moved during their execution.
        val runFolder = openOrCreateFolder(s"${queueFolder.getPath.toString}/run")
        // Clear out the "run" folder.
        runFolder.getMembers() foreach { mlist ⇒
            mlist foreach { mname ⇒
                runFolder getMember mname foreach { rfvfile ⇒
                    queueFolder getMember mname match {
                        case Full(vfile) ⇒
                            // File of same name already exists in the pending folder,
                            // so assume the script requeued itself before its execution
                            // was interrupted.
                            vfile close()
                        case Empty ⇒
                            // Move the file in the "run" folder back to the pending folder,
                            // on the assumption that since its execution was interrupted,
                            // it may not have completed its mission.
                            queueFolder link (mname, rfvfile) foreach (_ close ())
                        case f : Failure ⇒
                            Log.error(s"${runFolder.getPath.toString}/$mname error: ${f.messageChain}")
                    }
                    rfvfile close ()
                }
                runFolder unlink (mname, recursive = true)
            }
        }
        runFolder close ()
    }

    /**
     * The GroupPage table is deprecated. This looks for entries in GroupPage, converts them
     * to guest and home page settings in the group descriptor, and then deletes them.
     */
    def cleanGroupPages() : Unit = {
        GroupPage findAll () foreach { gpage ⇒
            gpage.page.obj foreach { pdef ⇒
                val pagestr = Option(pdef.page.get)
                val gid = ResourceId(gpage.group.get)
                GroupInfo(CfsVFileId(gid), SystemPrincipal) foreach { ginfo ⇒
                    val ptype = gpage.ptype.get
                    if (ptype == PageType.GUESTPAGE) {
                        ginfo setGuestPage pagestr foreach { _ ⇒ gpage.delete_! }
                    }
                    else if (ptype == PageType.HOMEPAGE) {
                        ginfo setHomePage pagestr foreach { _ ⇒ gpage.delete_! }
                    }
                    ginfo close ()
                }
            }
        }
    }

    def createSystemAttribute(name : String, atypeval : AttributeType.AttributeType, description : Option[String]) : Unit = {
        val okbox = AttrDef getAttributePath name flatMap { apath ⇒
            AttrDef openDefinition (apath, SystemPrincipal, CfsOpenOptions.Default) match {
                case Full(adef) ⇒
                    val result = adef.getData flatMap { ainfo ⇒
                        if (ainfo.atype == atypeval) Full(false)
                        else Failure(s"attribute already exists with type ${ainfo.atype}")
                    }
                    adef close ()
                    result
                case Empty ⇒
                    val options = AttrDefCreateOptions(atype = atypeval, description = description)
                    Cfs create (apath, SystemPrincipal, AttrDef.getMimeType, options) match {
                        case Full(adef : AttrDef) ⇒
                            val result = adef.getData map { _ ⇒ true }
                            adef close ()
                            result
                        case Full(other) ⇒
                            other close ()
                            Failure(s"system error: wrong file type")
                        case e : EmptyBox ⇒ e ?~! s"failed to create attribute file: $apath"
                    }
                case f : Failure ⇒ f ?~! s"$name may already exist but is not accessible"
            }
        }
        okbox match {
            case Full(true) ⇒ Log.info(s"created system attribute $name")
            case Full(false) ⇒
            case e : EmptyBox ⇒ Log.error(s"error creating system attribute $name: ${(e ?~ "Empty").messageChain}")
        }
    }
}
