/**
  * Copyright © 2016-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Created by Hep on 10/22/2016.
  */
package choice.script

import choice.access._
import choice.actor.DbManager
import choice.core.{Component, Startup}
import choice.core.Startup.PendingFolderPath
import choice.fs._
import choice.fs.vfs.VFile
import choice.lib.FileOps
import choice.lib.ExtendedBox._
import choice.model.ResourceId
import choice.parser.{CfsPathParser, CompParser}
import choice.script.js.Launcher
import choice.script.lua.LuaLauncher
import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.json.{DefaultFormats, Serialization}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers.{readWholeStream, tryo}

case class QueuedScript(script : String, principal : String, principalId : Long, asOwner : Boolean, args : String)

class CfsFileLib(principal : () ⇒ Principal) {
    implicit val formats : DefaultFormats.type = DefaultFormats

    private var folderStack : List[CfsAbsolutePath] = CfsRootPath :: Nil

    /**
      * Check for a valid file path. The path may be relative or absolute. This does
      * not take into account the current folder.
      *
      * @param path a supposed file path
      * @return true if the path syntax is valid, otherwise false
      */
    def isValidPath(path : String) : Boolean = {
        CfsPath(path) match {
            case Full(_) ⇒ true
            case _ ⇒ false
        }
    }

    /**
      * Check for a valid filename.
      *
      * @param name a supposed filename
      * @return true if the name syntax is valid, otherwise false
      */
    def isValidFilename(name : String) : Boolean = {
        CfsPathParser filename name match {
            case Full(_) ⇒ true
            case _ ⇒ false
        }
    }

    /**
      * Check for a valid absolute file path.
      *
      * @param path a supposed absolute file path
      * @return true if the path syntax is valid, otherwise false
      */
    def isAbsolutePath(path : String) : Boolean = {
        CfsPathParser abspath path match {
            case Full(_) ⇒ true
            case _ ⇒ false
        }
    }

    /**
      * Check for a valid relative file path. This does not take into account the
      * current folder.
      *
      * @param path a supposed relative file path
      * @return true if the path syntax is valid, otherwise false
      */
    def isRelativePath(path : String) : Boolean = {
        CfsPathParser relpath path match {
            case Full(_) ⇒ true
            case _ ⇒ false
        }
    }

    def getCurrentFolder : CfsAbsolutePath = folderStack.head

    def getAbsolutePath(path : String) : Box[CfsAbsolutePath] = {
        def helper(parts : List[String], acc : List[String]) : List[String] = {
            parts match {
                case Nil ⇒ acc.reverse
                case "." :: tail ⇒ helper(tail, acc)
                case ".." :: tail ⇒ helper(tail, acc drop 1)
                case head :: tail ⇒ helper(tail, head :: acc)
            }
        }
        val pathbox = Cfs.withValidPath(path) {
            case abspath : CfsAbsolutePath ⇒ Full(abspath)
            case relpath : CfsRelativePath ⇒
                val base = getCurrentFolder
                Full(CfsAbsolutePath(base.root, base.parts ++ relpath.allParts))
        }
        pathbox flatMap { abspath ⇒
            Full(CfsAbsolutePath(abspath.root, helper(abspath.parts, Nil)))
        }
    }

    def getcwd() : String = getCurrentFolder.toString

    /**
      * Lookup a file by its path, returning essential file metadata. If the path
      * is relative, it is assumed to be relative to the current folder.
      *
      * @param path file path, which may be relative to the current folder
      * @return a boxed map of metadata attributes and values
      */
    def lookup(path : String) : Box[Map[String, Any]] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal(), CfsOpenOptions.Default) {
                case cfsfile ⇒ Full(cfsfile.asMap + ("status" → 1))
            }
        }
    }

    /**
      * Lookup a file by its id, returning essential metadata.
      *
      * @param id the file's id, which is a ResourceId
      * @return a boxed map of metadata attributes and values
      */
    def lookupById(id : Long) : Box[Map[String, Any]] = {
        val rid = ResourceId(id)
        Cfs open (CfsVFileId(rid), principal(), CfsOpenOptions.Default) match {
            case Full(cfsfile : CfsFile) ⇒
                val result = Full(cfsfile.asMap + ("status" → 1))
                cfsfile close ()
                result
            case Full(vfile) ⇒
                vfile close ()
                Failure(s"resource id $id is not a Cfs file")
            case e : EmptyBox ⇒ e
        }
    }

    /**
      * List the files in a specified folder, subject to a MIME type filter. The
      * list will include entries for "." and "..", unless the MIME type filter
      * rejects them.
      *
      * @param path the folder path
      * @param mimetype an optional MIME type to filter the results
      * @return a boxed list of maps of metadata attributes and values
      */
    def list(path : String, mimetype : Option[String]) : Box[List[Map[String, Any]]] = {
        Cfs.withExistingFile(path, principal(), CfsOpenOptions.Default) {
            case folder : CfsFolder ⇒
                val mtfilter = mimetype map { mt ⇒
                    (file : VFile) ⇒
                        file match {
                            case cfsfile : CfsFile ⇒
                                (cfsfile.getMimeType.toOption fold false)(_ == mt)
                        }
                } getOrElse { _ : VFile ⇒ true }
                Full(FileOps.listFolder(folder, mtfilter))
        }
    }

    def mkdir(path : String, recursive : Boolean) : Box[Map[String, Any]] = {
        Cfs.withValidPath(path) { cfspath ⇒
            FileOps.makeFolder(cfspath, principal(), recursive = recursive)
        } map { folder ⇒
            val fmap = folder.asMap + ("status" → 1)
            folder close()
            fmap
        }
    }

    def rm(path : String, recursive : Boolean) : Box[Boolean] = {
        Cfs.withValidPath(path) { fpath ⇒
            val ppath = fpath.getParent
            Cfs.withExistingFile(ppath, principal()) {
                case pfile : CfsDirFile ⇒
                    pfile unlink (fpath.getFileName.toString, recursive)
            }
        }
    }

    def create(path : String, mimetype : String) : Box[Map[String, Any]] = {
        DbManager getMimeTypeHandler mimetype flatMap { mth ⇒
            if (mth.isSpecial_?) {
                Failure(s"operation not supported for MIME type $mimetype")
            }
            else {
                getAbsolutePath(path) flatMap { abspath ⇒
                    val options = CfsCreateOptions()
                    Cfs create(abspath, principal(), mimetype, options) flatMap {
                        case plain : CfsPlain ⇒
                            val result = Full(plain.asMap)
                            plain close()
                            result
                        case other ⇒
                            val ppath = abspath.getParent
                            Cfs.withExistingFile(ppath, SystemPrincipal, CfsOpenOptions.Default) {
                                case dirfile : CfsDirFile ⇒
                                    dirfile unlinkUnchecked (other, recursive = false)
                            }
                            other close ()
                            Failure(s"operation did not create a plain file")
                    }
                }
            }
        }
    }

    /**
      * Change the owner of a file to be the current principal or another specified principal.
      *
      * @param path path to the file
      * @param owner path to the new owner, if not the current principal
      * @return a boxed value of true if successful
      */
    def chown(path : String, owner : Option[String]) : Box[Boolean] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs open (abspath, principal(), CfsOpenOptions.Default) match {
                case Full(cfsfile : CfsFile) ⇒
                    val powner = owner match {
                        case Some(ownerpath) ⇒
                            getAbsolutePath(ownerpath) flatMap { absowner ⇒
                                Cfs.withExistingFile(absowner, principal(), CfsOpenOptions.Default) {
                                    case p : IsPrincipal ⇒ Full(p.getSelfPrincipal)
                                    case _ ⇒ Failure(s"$ownerpath is not a principal")
                                }
                            }
                        case None ⇒ Full(principal())
                    }
                    val result = powner flatMap { po ⇒
                        cfsfile chown po map { newfile ⇒
                            newfile close()
                            true
                        }
                    }
                    cfsfile close ()
                    result
                case Full(other) ⇒
                    other close ()
                    Failure("chown not supported on that file")
                case e : EmptyBox ⇒ e
            }
        }
    }

    def setMimeType(path : String, mimetype : String) : Box[Boolean] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal(), CfsOpenOptions.Default) {
                case cfsplain : CfsPlain ⇒
                    cfsplain setMimeType mimetype flatMap { _ ⇒
                        Full(true)
                    }
            }
        }
    }

    def share(policy : String, filespec : String) : Box[Map[String, Any]] = {
        Cfs.withExistingFile (policy, SystemPrincipal, CfsOpenOptions.Default) {
            case policy : Policy ⇒
                val list = policy protect (filespec, principal())
                Full(FileOps.processPolicyChangeList (list))
        }
    }

    /**
      * Set the current folder to a given path. The path must reference a folder.
      * It may contain "." and/or ".." components, which are interpreted as in
      * POSIX paths.
      *
      * @param path the folder path
      * @return a boxed absolute path corresponding to the given path
      */
    def cd(path : String) : Box[CfsAbsolutePath] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal()) {
                case folder : CfsFolder ⇒
                    val fpath = folder.getPath
                    folderStack = fpath :: (folderStack drop 1)
                    Full(fpath)
            }
        }
    }

    /**
      * Push a given folder on the current folder stack, making it the current
      * folder. The path must reference a folder. It may contain "." and/or ".."
      * components, which are interpreted as in POSIX paths.
      *
      * @param path the folder path
      * @return a boxed absolute path corresponding to the given path
      */
    def pushd(path : String) : Box[CfsAbsolutePath] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal()) {
                case folder : CfsFolder ⇒
                    val fpath = folder.getPath
                    folderStack = fpath :: folderStack
                    Full(fpath)
            }
        }
    }

    /**
      * Pop the current folder stack. If the last element is popped, the stack
      * is reinitialized to contain just the root path ("/").
      *
      * @return the current path
      */
    def popd() : CfsAbsolutePath = {
        folderStack match {
            case Nil ⇒
                // Shouldn't happen
                folderStack = CfsRootPath :: Nil
                CfsRootPath
            case head :: Nil ⇒
                folderStack = CfsRootPath :: Nil
                head
            case head :: tail ⇒
                folderStack = tail
                head
        }
    }

    /**
      * Queue a script for later execution. This stores a JSON file with the specified filename
      * in the /System/Pending folder. The scriptPath must identify an existing script file to
      * be executed in response to some event. The stored JSON file will be owned by the current
      * principal, which is also the principal that will be used for the subsequent script
      * execution. If a file with the specified filename already exists, the replace option
      * specifies whether it will be replaced. Only an administrator or the current owner of
      * the file can replace it.
      *
      * @param queue the queue name, corresponding to a folder in /System/Pending
      * @param filename the queued JSON filename
      * @param scriptPath the path to the script to be executed
      * @param args arguments for the script execution, possibly JSON-encoded
      * @param replace true if an existing queue element should be replaced
      * @return a box containing a Boolean, indicating whether an existing queue element was
      *         replaced
      */
    def queueScript(queue : String, filename : String, scriptPath : String, args : String,
                    replace : Boolean = true, asOwner : Boolean = false) : Box[Boolean] = {
        // Check that the specified queue exists
        Cfs.withValidFilename(queue) { queueName ⇒
            Cfs open (s"$PendingFolderPath/$queueName", SystemPrincipal, CfsOpenOptions.Default) match {
                case Full(queueFolder : CfsFolder) ⇒
                    // Check that the script file exists and that the current principal is able to run it.
                    // Nothing stops these conditions from changing before the script execution is requested,
                    // but then the script execution will fail.
                    val spathBox = getAbsolutePath(scriptPath) flatMap { spath ⇒
                        Cfs.withExistingFile(spath, principal(), CfsOpenOptions.Default) {
                            case scriptFile : CfsScriptFile ⇒
                                scriptFile.getMimeType flatMap { mimetype ⇒
                                    DbManager getMimeTypeHandler mimetype flatMap {
                                        case mth : ExecutableMimeType ⇒
                                            if (asOwner) mth.canRunAsOwner(scriptFile) { () ⇒ Full(spath) }
                                            else mth.canRun(scriptFile) { () ⇒ Full(spath) }
                                        case _ ⇒ Failure(s"${spath.toString} is not an executable MIME type")
                                    }
                                }
                        }
                    }
                    val result = spathBox flatMap { spath ⇒
                        val p = principal()
                        Cfs.withValidFilename(filename) { fname ⇒
                            val json = Serialization.write(QueuedScript(spath.toString, p.getPrincipalName, p.getPrincipalId.id,
                                asOwner, args))
                            queueFolder getMember fname match {
                                case Full(plain : CfsPlain) ⇒
                                    val result =
                                        if (replace) {
                                            if (plain.getMimeType.toOption.fold(false)(_ == "application/json")) {
                                                if (p.isSystemAdmin_? || plain.getVnode.getOwnerId == p.getPrincipalId) {
                                                    plain replaceFileData json map (_ ⇒ true)
                                                }
                                                else Failure(s"${p.getPrincipalName} does not own $fname")
                                            }
                                            else Failure(s"$fname already exists and is not application/json")
                                        }
                                        else Failure(s"$fname already exists and replace option is false")
                                    plain close()
                                    result
                                case Full(other) ⇒
                                    other close()
                                    Failure(s"$fname is not an application/json file")
                                case Empty ⇒
                                    queueFolder create (fname, "application/json", CfsCreateOptions.Default) match {
                                        case Full(plain : CfsPlain) ⇒
                                            val result = plain replaceFileData json map (_ ⇒ false)
                                            plain chown principal() foreach (_ close ())
                                            plain close()
                                            result
                                        case Full(other) ⇒
                                            queueFolder unlink (fname, recursive = false)
                                            other close()
                                            Failure(s"create operation did not produce a plain file")
                                        case e : EmptyBox ⇒ e
                                    }
                                case f : Failure ⇒ f
                            }
                        }
                    }
                    queueFolder close ()
                    result
                case Full(other) ⇒
                    other close ()
                    Failure(s"$queueName is not a valid queue")
                case Empty ⇒ Failure(s"queue $queueName does not exist")
                case f : Failure ⇒ f
            }
        }
    }

    /**
      * Retrieve a queued script with a given name from a specified queue.
      *
      * @param queue the queue name
      * @param filename the queued JSON filename
      * @return a boxed QueuedScript instance
      */
    def getQueuedScript(queue : String, filename : String) : Box[QueuedScript] = {
        Cfs.withValidFilename(queue) { queueName ⇒
            Cfs.withValidFilename(filename) { fname ⇒
                Cfs.withExistingFile(s"$PendingFolderPath/$queueName/$fname", SystemPrincipal, CfsOpenOptions.Default) {
                    case plain : CfsPlain ⇒
                        val p = principal()
                        val result =
                            if (p.isSystemAdmin_? || p.getPrincipalId == plain.getVnode.getOwnerId) {
                                tryo(CfsFiles.newBufferedReader(plain)) flatMap { rdr ⇒
                                    tryo(Serialization.read[QueuedScript](rdr))
                                }
                            }
                            else Failure(s"$fname: not owned by ${p.getPrincipalName}")
                        plain close ()
                        result
                }
            }
        }
    }

    def runQueuedScript(queue : String, filename : String,
                        event : String, eventArgs : Traversable[JValue]) : Box[(Int, Any)] = {
        // The script is run as the script file owner if asOwner is true. Otherwise it runs
        // as whoever queued it. The script is passed a userPrincipal argument, which is
        // the principal of the current user. The script is responsible for determining
        // whether the current user is authorized to run it.
        def getPrincipals(qscript : QueuedScript,
                          scriptFile : CfsScriptFile) : (CfsPrincipal, CfsPrincipal) = {
            val scriptOwner = CfsPrincipal(scriptFile.getVnode.getOwnerId)
            (if (qscript.asOwner) scriptOwner else CfsPrincipal(ResourceId(qscript.principalId)), scriptOwner)
        }

        def getArgs(queueName : String, filename : String,
                    qscript : QueuedScript, owner : CfsPrincipal) : Option[JArray] = {
            // Just one argument is passed to the script, an object containing various fields.
            val jo = JObject(List(
                JField("event", JString(event)),
                JField("eventArgs", JArray(eventArgs.toList)),
                JField("queue", JString(queueName)),
                JField("filename", JString(filename)),
                JField("script", JString(qscript.script)),
                JField("fileArgs", JString(qscript.args)),
                JField("userPrincipal", JString(principal().getPrincipalName)),
                JField("filePrincipal", JString(qscript.principal)),
                JField("scriptOwner", JString(owner.getPrincipalName)),
                JField("asOwner", JBool(qscript.asOwner))
            ))
            Some(JArray(List(jo)))
        }

        Cfs.withValidFilename(queue) { queueName ⇒
            Cfs.withValidFilename(filename) { fname ⇒
                Cfs.withExistingFile(s"$PendingFolderPath/$queueName", SystemPrincipal, CfsOpenOptions.Default) {
                    case qsFolder : CfsFolder ⇒
                        qsFolder.withMember("run") {
                            case qsRunFolder : CfsFolder ⇒
                                val qscriptBox = qsFolder.withMember(fname) {
                                    case plain : CfsPlain ⇒
                                        val result = tryo {
                                            val rdr = CfsFiles.newBufferedReader(plain)
                                            Serialization.read[QueuedScript](rdr)
                                        }
                                        // Move the queued file to the "run" folder. This is to allow the
                                        // script to queue itself or another script under the same name.
                                        // The original queued file is removed from the "run" folder after
                                        // its script has been executed.
                                        // link() returns a new handle, which is immediately closed.
                                        qsRunFolder link (fname, plain) foreach (_ close ())
                                        qsFolder unlink (fname, recursive = false)
                                        result
                                }
                                try {
                                    qscriptBox match {
                                        case Full(qscript) ⇒
                                            Cfs.withExistingFile(qscript.script, SystemPrincipal, CfsOpenOptions.Default) {
                                                case jscript : CfsJsScript ⇒
                                                    val (runPrincipal, ownerPrincipal) = getPrincipals(qscript, jscript)
                                                    val args = getArgs(queueName, fname, qscript, ownerPrincipal)
                                                    Launcher.run(jscript, runPrincipal, args, JsRunOptions())
                                                case lscript : CfsLuaScript ⇒
                                                    val (runPrincipal, ownerPrincipal) = getPrincipals(qscript, lscript)
                                                    val args = getArgs(queueName, fname, qscript, ownerPrincipal)
                                                    LuaLauncher.run(lscript, runPrincipal, args, LuaRunOptions())
                                            }
                                        case Empty ⇒ Failure(s"$fname script does not exist")
                                        case f : Failure ⇒ f
                                    }
                                }
                                finally {
                                    // Remove the queued file from the "run" folder
                                    qsRunFolder unlink(fname, recursive = false)
                                }
                        }
                }
            }
        }
    }

    def copy(fromPath : String, toPath : String, recursive : Option[Boolean] = None,
             withAttributes : Boolean = true, withPolicies : Boolean = true) : Box[CfsAbsolutePath] = {
        getAbsolutePath(fromPath) flatMap { fromAbs ⇒
            getAbsolutePath(toPath) flatMap { toAbs ⇒
                CfsFiles.copy(fromAbs, toAbs)(principal)
                Full(toAbs)
            }
        }
    }

    def writeJSON(path : String, json : String) : Box[Map[String, Any]] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs open (abspath, principal(), CfsOpenOptions.Default) match {
                case Full(plain : CfsPlain) ⇒
                    val result =
                        if (plain.getMimeType.toOption.fold(false)(_ == "application/json")) {
                            plain replaceFileData json map (_.asMap)
                        }
                        else Failure(s"$path already exists and is not application/json")
                    plain close ()
                    result
                case Full(other) ⇒
                    other close ()
                    Failure(s"${abspath.toString} is not an application/json file")
                case Empty ⇒
                    Cfs create (abspath, principal(), "application/json", CfsCreateOptions.Default) match {
                        case Full(plain : CfsPlain) ⇒
                            val result = plain replaceFileData json map (_.asMap)
                            plain close ()
                            result
                        case Full(other) ⇒
                            other close ()
                            Failure(s"create operation did not produce a plain file")
                        case e : EmptyBox ⇒ e
                    }
                case f : Failure ⇒ f
            }
        }
    }

    def readJSON(path : String) : Box[String] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal(), CfsOpenOptions.Default) {
                case plain : CfsPlain ⇒
                    if (plain.getMimeType.toOption.fold(false)(_ == "application/json")) {
                        tryo(CfsFiles.newInputStream(plain)) flatMap { in ⇒
                            tryo(new String(readWholeStream(in))) use { _ ⇒
                                // readWholeStream closes the stream, which closes the file
                                plain.setExpectedClosed()
                            }
                        }
                    }
                    else Failure(s"${abspath.toString} is not an application/json file")
            }
        }
    }

    def readSession(path : String) : Box[Map[String, Any]] = {
        getAbsolutePath(path) flatMap { abspath ⇒
            Cfs.withExistingFile(abspath, principal(), CfsOpenOptions.Default) {
                case sfile : SessionFolder ⇒
                    sfile.getData map { ssum ⇒
                        val props = List("version" → ssum.version, "startTime" → ssum.startTime,
                                         "ipAddress" → ssum.ipAddress,
                                         "sessionId" → ssum.sessionId, "userPath" → ssum.userPath,
                                         "userId" → ssum.userId, "userAgent" → ssum.userAgent)
                        val props2 = ssum.endTime map (("endTime", _) :: props) getOrElse props
                        val props3 = ssum.attachTime map (("attachTime", _) :: props2) getOrElse props2
                        val props4 = ssum.detachTime map (("detachTime", _) :: props3) getOrElse props3
                        Map(props4 : _*)
                    }
            }
        }
    }

    def sendMail(message : MailerMessage, mailer : Option[String]) : Box[Boolean] = {
        getAbsolutePath(mailer getOrElse Startup.SystemMailerPath) flatMap { mailerPath ⇒
            Cfs.withExistingFile(mailerPath, principal(), CfsOpenOptions.Default) {
                case mailer : CfsMailer ⇒
                    mailer.send(message) match {
                        case b @ Full(_) ⇒
                            mailer close()
                            b
                        case e : EmptyBox ⇒ e
                    }
            }
        }
    }

    def compileComponent(source : String, output : Option[String]) : Box[Map[String, Any]] = {
        getAbsolutePath(source) flatMap { sourcePath ⇒
            val self = principal()
            Cfs.withExistingFile(sourcePath, self, CfsOpenOptions.Default) {
                case sfile : CfsPlain ⇒
                    val outname = output getOrElse {
                        val parts = sfile.getName.split('.')
                        // If the source file has an extension, the default output file is the same
                        // name without the extension. Otherwise it's the same name with a ".cco".
                        val oname = if (parts.length > 1) parts.dropRight(1).mkString(".")
                        else s"${sfile.getName}.cco"
                        (sfile.getPath.getParent / oname).toString
                    }
                    (CfsPathParser.path(outname, CfsRootRoot) match {
                        case Full(abspath : CfsAbsolutePath) ⇒ Full(abspath)
                        case Full(relpath : CfsRelativePath) ⇒ Full(sfile.getPath.getParent resolve relpath)
                        case e : EmptyBox ⇒ e
                        case other ⇒ sys.error(s"unexpected output path $other")
                    }) flatMap { outpath ⇒
                        val omember = outpath.getFileName.toString
                        Cfs.withExistingFile(outpath.getParent, self) {
                            case ofolder : CfsFolder ⇒
                                tryo(CfsFiles.newInputStream(sfile)) flatMap { instream ⇒
                                    import choice.parser.CompParser.{NoSuccess, Success}

                                    //val src = Source.fromInputStream(instream)
                                    val src = new String(Helpers.readWholeStream(instream))
                                    //val pseq = PagedSeq.fromSource(src)
                                    //val rdr = new PagedSeqReader(pseq)
                                    CompParser.parseAll(CompParser.component, src) match {
                                        case Success(cdesc, _) ⇒
                                            val tmpname = FileOps.getRandomFilename(ofolder)
                                            ofolder create(tmpname, Component.getMimeType, CfsCreateOptions.Default) match {
                                                case Full(comp : Component) ⇒
                                                    comp putData cdesc flatMap { _ ⇒
                                                        FileOps.replaceFile(ofolder, omember, comp) match {
                                                            case Full(c) ⇒
                                                                val map = c.asMap ++ Map("status" → 1)
                                                                c close()
                                                                Full(map)
                                                            case e : EmptyBox ⇒
                                                                comp close()
                                                                e
                                                        }
                                                    }
                                                case Full(vfile) ⇒
                                                    vfile close()
                                                    ofolder unlink(tmpname, false)
                                                    Failure(s"component creation returned unexpected file type")
                                                case e : EmptyBox ⇒ e
                                            }
                                        case NoSuccess(msg, _) ⇒ Failure(s"syntax error: $msg")
                                    }
                                }
                        }
                    }
            }
        }
    }
}
