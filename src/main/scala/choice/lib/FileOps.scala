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
  * Web interface for file operations.
  *
  * @author Howard Palmer
  */
package choice.lib

import java.io.{BufferedInputStream, InputStream, InputStreamReader}
import java.nio.file._
import java.util
import java.util.zip.ZipEntry

import _root_.net.liftweb._
import choice.access._
import choice.actor._
import choice.attributes.{AttrDef, AttrDefCreateOptions, AttrVal}
import choice.core._
import choice.fs.archive._
import choice.fs.vfs.{AttributeId, AttributeType, CanHazMap, VFile}
import choice.fs.{CfsRootPath, _}
import choice.lib.ExtendedBox._
import choice.lib.JsonHelpers._
import choice.model._
import choice.script.CfsFileLib
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json.Extraction._
import net.liftweb.json._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

import scala.collection.immutable.Stream
import scala.util.matching.Regex

/**
 * Base class for a 'replace' option on file operations.
 */
sealed abstract class ReplaceOption {
    /**
     * Determine whether the specified existing file should be replaced by another
     * file. Attributes of the other file needed to make the determination are passed
     * as case class arguments.
     *
     * @param cfsfile an existing file that might be replaced
     * @return a boolean indicating whether the existing file should be replaced
     */
    def replace_?(cfsfile : CfsFile) : Boolean
}

object ReplaceOption {
    def apply(replace : Option[String], lmtime : Option[Long]) : ReplaceOption = {
        val lcreplace = replace map (_.toLowerCase)
        (lcreplace, lmtime) match {
            case (Some("always"), _) ⇒ ReplaceAlways
            case (Some("never"), _) ⇒ ReplaceNever
            case (Some("older"), Some(t)) ⇒ ReplaceOlder(t)
            case _ ⇒ ReplaceNever
        }
    }
}

/**
 * ReplaceOption to always replace the existing file.
 */
object ReplaceAlways extends ReplaceOption {
    def replace_?(cfsfile : CfsFile) : Boolean = true
}

/**
 * ReplaceOption to never replace the existing file.
 */
object ReplaceNever extends ReplaceOption {
    def replace_?(cfsfile : CfsFile) : Boolean = false
}

/**
 * ReplaceOption to replace the existing file if and only if its last modified
 * time is strictly older than a specified time.
 *
 * @param lmtime the last modified time of the potential replacement file
 */
case class ReplaceOlder(lmtime : Long) extends ReplaceOption {
    def replace_?(cfsfile : CfsFile) : Boolean = {
        cfsfile info None match {
            case Full(info) ⇒ lmtime > info.mtime
            case _ : EmptyBox ⇒ false
        }
    }
}

object FileOps {
    implicit val formats : DefaultFormats.type = DefaultFormats

    private val Log = Logger("choice.lib.fileops")
        
//    case class TmpFile(name : String, contentType : String, cfph : ChoiceFileParamHolder)

    def sameMimeTypes(file1 : CfsFile, file2 : CfsFile) : Boolean = {
        file1.getMimeType flatMap { mt1 ⇒ file2.getMimeType flatMap { mt2 ⇒ Full(mt1 == mt2) }} openOr false
    }

    def isMimeType_?(file : CfsFile, mimeType : String) : Boolean = {
        file.getMimeType flatMap { mt ⇒ Full(mt == mimeType) } openOr false
    }

    /**
     * This is used by commands where there is a target folder and a filename for some file
     * that is being created. If toFolder is specified, then the folder it references must
     * exist, and it is returned as the target folder. If toFolder is not specified, then
     * a user-specific temporary folder is designated as the target folder, and is created
     * if it doesn't already exist.
     *
     * The defaultName is always specified, and used as the returned filename if toName
     * is not specified. Otherwise toName is returned as the target filename.
     *
     * @param principal the principal responsible for this operation
     * @param defaultName the default filename for the target file
     * @param toName an optional preferred filename for the target file
     * @param toFolder an optional file path for the target folder
     * @return a boxed tuple containing the target filename and folder, or Failure if
     *         the target folder does not exist
     */
    def getTargetFolder(principal : Principal, defaultName : String,
                        toName : Option[String], toFolder : Option[String]) : Box[(String, CfsFolder)] = {
        val targetFolder = toFolder match {
            case Some(fpath) ⇒ Cfs.openFolderPath (fpath, principal)
            case None ⇒ getTempFolder(principal)
        }
        val targetName = toName getOrElse defaultName
        targetFolder map ((targetName, _))
    }

    /**
     * Get a folder for temporary files for a given user. The folder will be created if necessary.
     *
     * @param principal the user for which a temporary file folder is needed
     * @return a box temporary folder
     */
    def getTempFolder(principal : Principal) : Box[CfsFolder] = {
        getOrMakeFolder(getTempPath(principal), principal, recursive = true)
    }

    def getTempPath(principal : Principal) : CfsAbsolutePath = {
        val userid =
            if (principal eq GuestPrincipal) {
                val sessionId = SessionManager.getSessionClient(true) map (_.sessionId.id)
                s"guest-${sessionId getOrElse "None"}"
            }
            else {
                val pname = principal.getPrincipalName
                pname substring ((pname lastIndexOf '/') + 1)
            }
       CfsAbsolutePath(CfsRootRoot, List("Temp", userid))
    }

    def getRandomFilename(folder : CfsFolder) : String = {
        def helper(failed : Int) : String = {
            val membername = s"tmp${randomString(16)}"
            // There might be some reason why getMember fails that is independent
            // of the member name. Give up if that seems to be the case.
            if (failed >= 3) membername
            else folder getMember membername match {
                case Full(vfile) ⇒
                    vfile close ()
                    helper (failed)
                case Empty ⇒ membername
                case _ : Failure ⇒ helper(failed + 1)
            }
        }
        helper(0)
    }

    def makeTempFile(folder : CfsFolder, dataSeqnum : Long,
                     mimetype : String, modtime : Long = millis) : Box[CfsFile] = {
        val options = CfsCreateOptions(dataSeqnum = dataSeqnum, ctime = modtime)
        folder create (getRandomFilename(folder), mimetype, options)
    }

    /**
     * Copy policies from an old file to a new file. Typically this is used when the
     * new file is replacing the old file.
     *
     * @param oldfile the old file
     * @param newfile the new file
     */
    def copyPolicies(oldfile : CfsFile, newfile : CfsFile) : Unit = {
        val oldvnode = oldfile.getVnode
        val newvnode = newfile.getVnode
        oldvnode.getPolicyRefs foreach (newvnode addPolicy _.getPolicy)
    }

    /**
      * Replace an existing file with data from an InputStream. This is valid only
      * for plain files or script files. The specified MIME type does not have to
      * match the MIME type of the existing plain file, but it does have to match
      * if the existing file is a supported script type.
      *
      * This does not change the identity of the replaced file, only its contents,
      * and possibly its MIME type and modify time. Any access control and/or file
      * metadata attributes associated with the file will remain with it.
      *
      * @param in the input data to replace the existing file
      * @param mimetype the MIME type of the input data, and the resulting MIME
      *                 type of the output file if the operation succeeds
      * @param tfolder the folder containing the output file
      * @param curfile the file to be replaced
      * @param modtime the modify time to be set on the output file, default to
      *                the current time
      * @return
      */
    def replaceFileAlt(in : InputStream, mimetype : String,
                       tfolder : CfsFolder, curfile : CfsFile, modtime : Option[Long]) : Box[CfsFile] = {
        val outpath = curfile.getPath
        val principal = curfile.getPrincipal
        implicit val principalImpl : () ⇒ Principal = () ⇒ principal
        val replaceOk = try {
            val curmt = curfile.getMimeType.openOr("")
            curfile match {
                case cfsplain : CfsPlain ⇒
                    if (cfsplain.isInstanceOf[ExecutableMimeType] && curmt != mimetype) {
                        Failure(s"cannot replace ${outpath.toString} with a different MIME type")
                    }
                    else {
                        val mtok =
                            if (curmt != mimetype) cfsplain.setMimeType(mimetype)
                            else Full(mimetype)
                        mtok map { mt ⇒
                            CfsFiles.copy(in, outpath, StandardCopyOption.REPLACE_EXISTING, MIME_TYPE(mt))
//                            in close ()
                        }
                    }
                case _ ⇒ Failure(s"cannot replace ${outpath.toString} with MIME type $curmt")
            }
        }
        catch {
            case ex : Throwable ⇒
                Failure(s"error replacing ${outpath.toString}: ${ex.getMessage}")
        }
        finally {
            curfile close ()
        }
        replaceOk flatMap { copylen ⇒
            Cfs open (outpath, principal, CfsOpenOptions.Default) match {
                case Full(cfsfile : CfsFile) ⇒
                    // Do some consistency checking
                    val vnode = cfsfile.getVnode
                    val resource = vnode.getResource
                    val reslen = resource.getSize
                    if (copylen != reslen) {
                        Log.error(s"replaceFileAlt: copylen $copylen != reslen $reslen")
                    }
                    resource.getDataNode match {
                        case Full(dnode) ⇒
                            if (dnode.seqnum.get != resource.getSeqnum) {
                                Log.error(s"replaceFileAlt: inconsistency on ${outpath.toString}")
                            }
                        case _ ⇒
                            if (reslen != 0) {
                                Log.error(s"replaceFileAlt: ${outpath.toString}, length $reslen has no DataNode")
                            }
                    }
                    Full(cfsfile)
                case Full(other) ⇒
                    other close ()
                    Failure(s"replacement of ${outpath.toString} went badly")
                case e : EmptyBox ⇒ e
            }
        }
    }

    /**
     * The goal of this function is to link a file into a folder with a specified name.
     * If another file with the same name is already in the folder, it is unlinked first.
     * If the specified file is successfully linked under the given name, it is unlinked
     * from its old name, which is assumed to be a temporary name in the same folder.
     *
     * @param folder a folder containing newfile
     * @param member the permanent name to be assigned to newfile
     * @param newfile a file in folder, typically under a temporary name
     * @tparam T the type of newfile
     * @return boxed file handle for newfile, but under the new name
     */
    def replaceFile[T <: CfsFile](folder : CfsFolder, member : String, newfile : T) : Box[T] = {
        val tmpname = newfile.getName
        // Helper to replace a file already using the desired permanent name
        def doReplace(oldfile : CfsFile) : Box[T] = {
            // Copy access control policies to the new file
            copyPolicies(oldfile, newfile)
            // Unlink the file currently using the permanent name
            val result = folder unlink (member, false) flatMap { _ ⇒
                // Link newfile under its permanent name
                folder link (member, newfile) match {
                    case Full(nf : CfsFile) ⇒
                        // Unlink newfile from its temporary name and close it
                        folder unlink (tmpname, false)
                        newfile close ()
                        // Return a handle for newfile under its permanent name
                        Full(nf.asInstanceOf[T])
                    case Full(vfile) ⇒
                        // This is unexpected. Try to put things back.
                        folder unlink (member, false)
                        vfile close ()
                        folder link (member, oldfile) foreach (_ close ())
                        Failure(s"cannot replace $member with a non-Cfs file")
                    case e : EmptyBox ⇒
                        folder link (member, oldfile) foreach (_ close ())
                        e
                }
            }
            oldfile close ()
            result
        }
        // Is the specified member name already in use?
        folder getMember member match {
            case Full(f : CfsFolder) ⇒
                f close ()
                Failure(s"cannot replace folder $member")
            case Full(cfsfile : CfsPlain) ⇒ doReplace(cfsfile)
            case Full(cfsfile : CfsFile) if sameMimeTypes(cfsfile, newfile) ⇒
                doReplace(cfsfile)
            case Full(vfile) ⇒
                vfile close ()
                Failure(s"cannot replace non-Cfs file $member")
            case Empty ⇒
                // No, just link newfile as member, and unlink it from its temporary name
                folder link (member, newfile) match {
                    case Full(nf : CfsFile) ⇒
                        folder unlink (tmpname, false)
                        newfile close ()
                        Full(nf.asInstanceOf[T])
                    case Full(vfile) ⇒
                        folder unlink (member, false)
                        vfile close ()
                        Failure(s"link $member produced unexpected non-Cfs file")
                    case e : EmptyBox ⇒
                        folder unlink (tmpname, false)
                        e
                }
            case f : Failure ⇒ f
        }
    }

    def init() : Unit = {
        
        //Migration()
        updateDefaultDirectory()
//        extractDbRoot()
//        DbManager reviewCache ()

        // Testing
        // TODO: remove
        //extractFolder(List("home", "hep"))

        LiftRules.dispatch.prepend {
            // register the file operations handler
            case r @ Req(_, _, GetRequest | PostRequest) if r.param("api") == Full("file") ⇒
                if (r.post_?) () ⇒ handleOp(r)
                else () ⇒ handleGet(r)
        }
    }

//    def getMimeType(filename : String) : String = {
//        val result = http.ResourceServer.detectContentType(filename)
//        Log.info(s"getMimeType: file $filename, result $result")
//        result
//    }
        
//    def getMimeType(path : Path) : String = {
//        getMimeType(path.getFileName.toString)
//    }
    
    /**
     * Get a Path object for the host directory that is the root of this context.
     */
    def getContextPath : Path = {
        val path = http.LiftRules.context match {
            case c : net.liftweb.http.provider.servlet.HTTPServletContext ⇒
                c.ctx.getRealPath("/")
            case _ ⇒ "."
        }
        Paths.get(path).toAbsolutePath
    }
    
//    /**
//     * Get a Path object for a file in the server context directory of the
//     * host filesystem.
//     *
//     * @param path the path
//     * @return the full host path
//     */
//    def getHostPath(path : String*) : Path = {
//        getContextPath.resolve(getRelativePath(path : _*))
//    }

//    def getHostPath(path : Path) : Path = {
//        getContextPath.resolve(path)
////        if (result.toString.contains("fschoice")) result
////        else {
////            Log.error("getHostPath bad path: " + result.toString)
////            throw new RuntimeException("getHostPath")
////        }
//    }

//    def getRelativePath(path : String*) : Path = {
//        path.toList match {
//            case Nil ⇒ Paths.get("")
//            case first :: Nil ⇒ Paths.get(first)
//            case head :: tail ⇒ Paths.get(head, tail : _*)
//        }
//    }
    
//    def getRelativePath(dbfile : CfsFile) : Path = {
//        getRelativePath(dbfile.getPath.allParts : _*)
//    }

//    /**
//     * Recursively extract a DB Folder to a host directory.
//     *
//     * @param path  the folder path, which also will define the path
//     *              to the directory in the host filesystem
//     * @return true if successful, false otherwise
//     */
//    def extractFolder(path : String, by : UserInfo) : Boolean = {
//        Cfs open (CfsPath(path), by.getSelfPrincipal) match {
//            case Full(folder : CfsFolder) ⇒ extractFolder(folder, getRelativePath(path))
//            case Full(_) ⇒ false
//            case _ ⇒ false
//        }
//    }

//    def extractFolder(dbfile : CfsFile) : Boolean = {
//        dbfile match {
//            case folder : CfsFolder ⇒ extractFolder(folder, getRelativePath(dbfile))
//            case _ ⇒ false
//        }
//    }

//    def extractFolder(folder : CfsFolder, path : List[String]) : Boolean = {
//        extractFolder(folder, getRelativePath(path : _*))
//    }
    
//    /**
//     * Recursively extract a DB Folder to a host directory.
//     *
//     * @param folder    the DB Folder to be extracted
//     * @param path  the components of the folder path, which also will define the path
//     *              to the directory in the host filesystem
//     * @return true if successful, false otherwise
//     */
//    def extractFolder(folder : CfsFolder, path : Path) : Boolean = {
//        val dirpath = getHostPath(path)
//        Log.debug("begin extracting folder " + folder.getPath + " to " + dirpath.toString)
//        extractPath(dirpath)
//        val (folders, files) = folder.getFoldersAndFiles
//        files.foreach { f ⇒
//            val fpath = path.resolve(f.getName)
//            Log.debug("extracting file " + fpath.toString)
//            extractFile(f, fpath)
//            f.close()
//        }
//        Log.debug(folders.size + " sub-folders to extract")
//        folders.foreach { f ⇒
//            extractFolder(f, path.resolve(f.getName))
//            f.close()
//        }
//        true
//    }

//    /**
//     * Recursively create a directory in the host file system.
//     *
//     * @param path  the components of the path to the host directory to be created,
//     *              relative to the context
//     * @return true if successful, false otherwise
//     */
//    def extractPath(path : List[String]) : Boolean = {
//        if (path == null) {
//            Log.error("path is null")
//        }
//        extractPath(getHostPath(path : _*))
//    }

//    def extractPath(path : Path) : Boolean = {
//        val apath = if (path.isAbsolute) path else getHostPath(path)
//        if (Files.isDirectory(apath, LinkOption.NOFOLLOW_LINKS)) {
//            Log.debug("extractPath directory exists: " + apath)
//            true
//        }
//        else tryo(Files.createDirectories(apath)) match {
//            case Full(p) ⇒
//                Log.debug("extractPath created " + p)
//                true
//            case e : EmptyBox ⇒
//                Log.error("extractPath failed to create " + apath, e)
//                false
//        }
//    }

//    /**
//     * Extract a DB file to the host filesystem.
//     *
//     * @param fsobj     the DB file to extract
//     * @param path      the components of the host file path
//     * @return true if successful, false otherwise
//     */
//    def extractFile(fsobj : VFile, path : Path) : Boolean = {
//        fsobj match {
//            case plain : CfsPlain ⇒
//                Box !! path.getParent match {
//                    case Full(parent) ⇒
//                        Log.debug("ensuring existence of folder " + parent.toString)
//                        extractPath(parent)
//                    case _ ⇒
//                }
//                val hpath = getHostPath(path)
//                val result : Box[Boolean] = plain info None flatMap { info ⇒
//                    if (!Files.exists(hpath) || Files.getLastModifiedTime(hpath).toMillis < info.mtime) tryo {
//                        plain.getInputStream map { fsin ⇒
//                        //val openopt = List(CREATE, TRUNCATE_EXISTING, WRITE)
//                            Files.copy(fsin, hpath, StandardCopyOption.REPLACE_EXISTING)
//                            //Files.write(hpath, fsobj.getBytes, openopt : _*)
//                            Files.setLastModifiedTime(hpath, FileTime.fromMillis(info.mtime))
//                            true
//                        } openOr {
//                            Log.error(s"failed to open ${plain.getPath.toString}")
//                            false
//                        }
//                    }
//                    else {
//                        Log.debug(hpath.toString + " not replaced")
//                        Full(false)
//                    }
//                }
//                result match {
//                    case Full(b) ⇒ b
//                    case e : EmptyBox ⇒
//                        Log.error("error creating " + hpath, e)
//                        false
//                }
//            case _ ⇒ false
//        }
//    }
    
//    def extractFile(fsobj : CfsFile, path : String) : Boolean = {
//        val adjpath = if (path.startsWith("/")) path.substring(1) else path
//        extractFile(fsobj, getRelativePath(adjpath))
//    }

    def getOrMakeFolder(path : CfsPath, by : Principal, recursive : Boolean = false,
                        crtime : Long = millis) : Box[CfsFolder] = {
        Cfs.openFolder (path, by) match {
            case full @ Full(_) ⇒ full
            case _ : EmptyBox ⇒ makeFolder(path, by, recursive, crtime)
        }
    }

    /**
     * Create a folder in the DB filesystem, optionally recursively.
     *
     * @param path  the path to the folder to be created
     * @param recursive true for recursive create of missing parent folders
     * @param crtime    creation time for the new folder
     * @return the new folder, boxed, if successful, otherwise Failure
     */
    def makeFolder(path : CfsPath, by : Principal, recursive : Boolean = false,
                   crtime : Long = System.currentTimeMillis) : Box[CfsFolder] = {
        def getParentPaths(path : CfsPath, parents : List[CfsPath]) : List[CfsPath] = {
            val parent = path.getParent
            if (parent == path) parents
            else getParentPaths(parent, parent :: parents)
        }
        def createParents(parents : List[CfsPath], last : Box[CfsFolder]) : Box[CfsFolder] = {
            parents match {
                case Nil ⇒ last
                case head :: tail ⇒
                    Cfs.openFolder (head, by) match {
                        case Full(folder) ⇒
                            folder close ()
                            createParents(tail, last)
                        case Empty ⇒
                            Cfs create (head, by, CfsFolder.getMimeType, CfsCreateOptions.Default) match {
                                case Full(child : CfsFolder) ⇒
                                    last foreach (_ close ())
                                    createParents(tail, Full(child))
                                case Full(vfile) ⇒
                                    vfile close ()
                                    Failure(s"unexpected result from creating folder ${head.toString}")
                                case e : EmptyBox ⇒ e
                            }
                        case f : Failure ⇒ f
                    }
            }
        }
        Log.debug(s"makeFolder: ${path.toString}, recursive: $recursive")
        if (recursive) {
            val parents = getParentPaths(path, List(path))
            createParents(parents, Empty) match {
                case full @ Full(_) ⇒ full
                case Empty ⇒ Failure(s"$path already exists")
                case f : Failure ⇒ f
            }
        }
        else {
            val checkExists : Box[CfsPath] = Cfs.openFolder (path, by) { vfile ⇒
                vfile close ()
                Failure(s"${path.toString} already exists, but is not a folder")
            } flatMap { folder ⇒
                folder close ()
                Failure(s"${path.toString} already exists")
            }
            checkExists ifempty Full(path) flatMap { tpath ⇒
                val parent = tpath.getParent
                Cfs.withExistingFile(parent, by) {
                    case pfolder : CfsFolder ⇒
                        pfolder create(tpath.getFileName.toString, CfsFolder.getMimeType, CfsCreateOptions.Default) match {
                            case Full(folder : CfsFolder) ⇒ Full(folder)
                            case Full(vfile) ⇒
                                vfile close ()
                                Failure(s"unexpected result from creating folder ${tpath.toString}")
                            case e : EmptyBox ⇒ e
                        }
                }
            }
        }
    }

    /**
     * Lookup a file by path or by file id, and return information about it. By default, a Failure
     * is returned if the file does not exist. However, if 'exists' is specified as False, it
     * simply returns a status of zero with a message indicating the file does not exist.
     *
     * @param path the full path to the file
     * @param id the file id
     * @param exists a boolean indicating whether the file is expected to exist, defaulting
     *               to true
     */
    sealed case class AjaxLookupPath(path : Option[String], id : Option[Long], exists: Option[Boolean])
        extends AjaxApiRequest("file", "lookup") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val must_exist = exists getOrElse true
            val resultMap =
                if (path.isDefined && id.isDefined) Failure("specify path or id, not both")
                else path match {
                    case Some(pathstr) ⇒
                        Cfs open (pathstr, self, CfsOpenOptions.Default) match {
                            case Full(vfile) ⇒
                                val result = vfile match {
                                    case chm : CanHazMap ⇒ Full(chm.asMap)
                                    case _ ⇒ Failure("file does not support lookup")
                                }
                                vfile close ()
                                result
                            case Empty ⇒
                                val msg = s"$pathstr does not exist"
                                if (must_exist) Failure(msg)
                                else Full(Map[String, Any]("status" → 0, "msg" → msg))
                            case f : Failure ⇒ f
                        }
                    case None ⇒ id match {
                        case Some(resid) ⇒
                        	val rid = ResourceId(resid)
                            Cfs open (CfsVFileId(rid), self, CfsOpenOptions.Default) match {
                                case Full(cfsfile : CfsFile) ⇒
                                    val paths = cfsfile.findPaths map { path ⇒ path.toString }
                                    val result = Full(cfsfile.asMap + ("paths" → paths))
                                    cfsfile close ()
                                    result
                                case Full(vfile) ⇒
                                    vfile close ()
                                    Failure(s"resource id $resid is not a Cfs file")
                                case Empty ⇒
                                    val msg = s"file id $resid does not exist"
                                    if (must_exist) Failure(msg)
                                    else Full(Map[String, Any]("status" → 0, "msg" → msg))
                                case f : Failure ⇒ f
                            }
                        case None ⇒ Failure("nothing to lookup")
                    }
                }
            resultMap match {
                case Full(map) ⇒ MapResponse(map)
                case Empty ⇒ SimpleResponse(0, "not found")
                case f : Failure ⇒ FailureResponse(f)

            }
        }
    }

    def listFolder(cfsfolder : CfsFolder, filterfn : VFile ⇒ Boolean) : List[Map[String, Any]] = {
        val (folderList, fileList) = cfsfolder.getFoldersAndFiles
        val list = ((folderList ::: fileList) filter filterfn) map { chfile ⇒
            val chname = chfile.getName
            chfile match {
                case cfsch : CfsFile ⇒ cfsch.asMap + ("name" → chname) + ("status" → 1)
                case _ ⇒ Map[String, Any]("name" → chname, "status" → 1)
            }
        }
        folderList foreach (_.close())
        fileList foreach (_.close())
        val withDotDot = {
            Cfs open (cfsfolder.getPath.getParent, cfsfolder.getPrincipal, CfsOpenOptions.Default) match {
                case Full(parent : CfsFile) ⇒
                    val result =
                        if (filterfn(parent)) (parent.asMap + ("name" → "..") + ("status" → 1)) :: list
                        else list
                    parent close ()
                    result
                case Full(other) ⇒
                    other close ()
                    list
                case _ ⇒ list
            }
        }
        val withDot =
            if (filterfn(cfsfolder)) (cfsfolder.asMap + ("name" → ".") + ("status" → 1)) :: withDotDot
            else withDotDot
        withDot
    }

    sealed case class AjaxListFolder(path : String, folder : Option[Boolean]) extends AjaxApiRequest("file", "list") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val fopt = folder.getOrElse(false)
                Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                    case cfsfolder : CfsFolder if !fopt ⇒ Full(listFolder(cfsfolder, _ ⇒ true))
                    case dbfile : CfsFile ⇒
                        Full(List(dbfile.asMap + ("name" → dbfile.getName) + ("status" → 1)))
                    case _ ⇒ Failure("unexpected error from lookup")
                } match {
                    case Full(mlist) ⇒ ArrayResponse(mlist)
                    case Empty ⇒ SimpleResponse(-1, "unexpected Empty result")
                    case f : Failure ⇒ FailureResponse(f)
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxListType(mimetype : String, path : Option[String]) extends AjaxApiRequest("file", "mtlist") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val checkPath = path map { p ⇒
                if (p == "/") { (_ : CfsFile) ⇒ true }
                else {
                    val prefix = if (p.endsWith("/")) p else s"$p/"
                    (f : CfsFile) ⇒ {
                        DbManager findPaths(f.getVnode, self) match {
                            case Full(plist) ⇒ plist exists (_.toString.startsWith(prefix))
                            case _ : EmptyBox ⇒ false
                        }
                    }
                }
            } getOrElse { (_ : CfsFile) ⇒ true }
            val mtid = DbManager getMimeTypeId mimetype
            val resIds = Resource findAll By(Resource.mtid, mtid) map (_.getSafeKey)
            def helper(idlist : List[ResourceId], maplist : List[Map[String, Any]]) : List[Map[String, Any]] = {
                idlist match {
                    case Nil ⇒ maplist
                    case head :: tail ⇒
                        Cfs open (CfsVFileId(head), self, CfsOpenOptions.Default) match {
                            case Full(cfsfile : CfsFile) ⇒
                                if (checkPath(cfsfile)) {
                                    val fmap = cfsfile.asMap
                                    cfsfile close ()
                                    helper(tail, if (fmap.isEmpty) maplist else fmap :: maplist)
                                }
                                else helper(tail, maplist)
                            case Full(vfile) ⇒
                                vfile close ()
                                helper(tail, maplist)
                            case _ : EmptyBox ⇒ helper(tail, maplist)
                        }
                }
            }
            MapResponse(Map("status" → 1, "files" → helper(resIds, Nil)))
        }
    }

    sealed case class AjaxSetMimeType(path : String, mimetype : String) extends AjaxApiRequest("file", "setmt") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile (path, self, CfsOpenOptions.Default) {
                case cfsplain : CfsPlain ⇒
                    cfsplain setMimeType mimetype flatMap { newmt ⇒
                        MapResponse(Map("status" → 1, "mimetype" → newmt))
                    }
            }
        }
    }

    sealed case class AjaxFileCompletion(path : String, container : Option[Boolean])
        extends AjaxApiRequest("file", "fcmp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val endslash = path endsWith "/"
            Cfs.withValidPath(path) { cfspath ⇒
                val apath = CfsRootPath resolve cfspath
                findDir (apath, self) match {
                    case Full(dir) ⇒
                        val dirpath = dir.getPath
                        val matches = (apath.allParts.length - dirpath.allParts.length match {
                            case n if n == 0 ⇒
                                if (endslash) dir getMembers () map (_.toList)
                                else Full(List(""))
                            case n if n == 1 ⇒
                                val prefix = apath.allParts.last
                                dir getMembers () map { members ⇒
                                    (members filter (_.startsWith(prefix))).toList
                                }
                            case _ ⇒ Empty
                        }).toOption
                        val cmatches = container.fold(matches) { copt ⇒
                            if (copt) matches map { list ⇒
                                list filter { name ⇒
                                    if (name == "") true
                                    else {
                                        dir getMember name match {
                                            case Full(cfsfile : CfsFile) ⇒
                                                val keep = cfsfile.isContainer_?
                                                cfsfile close ()
                                                keep
                                            case Full(other) ⇒
                                                other close ()
                                                false
                                            case _ : EmptyBox ⇒ false
                                        }
                                    }
                                }
                            }
                            else matches
                        }
                        dir close ()
                        val status = (matches fold 0)(_.length)
                        MapResponse(Map("status" → status, "dir" → dirpath.toString, "matches" → cmatches))
                    case e : EmptyBox ⇒ e
                }
            }
        }

        private def findDir(path : CfsAbsolutePath, principal : Principal) : Box[CfsDirFile] = {
            Cfs open (path, principal, CfsOpenOptions.Default) match {
                case Full(dir : CfsDirFile) ⇒ Full(dir)
                case Full(other) ⇒
                    other close ()
                    findDir (path.getParent, principal)
                case Empty ⇒ findDir (path.getParent, principal)
                case f : Failure ⇒
                    if ((path compareTo CfsRootPath) == 0) f
                    else findDir (path.getParent, principal)
            }
        }
    }

    sealed case class AjaxMakeFolder(path : String, recursive : Option[Boolean])
        extends AjaxApiRequest("file", "mkdir") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(path) { fpath ⇒
                    makeFolder(fpath, self, recursive.getOrElse(false)) match {
                        case Full(fsn) ⇒
                            val result = fsn.asMap
                            fsn close ()
                            MapResponse(result)
                        case Empty ⇒ SimpleResponse(-1, "unexpected Empty result")
                        case f : Failure ⇒ FailureResponse(f)
                    }
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxLink(srcpath : String, dstpath : String, rename : Option[Boolean])
        extends AjaxApiRequest("file", "link") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val rename = this.rename getOrElse false
            Cfs.withExistingFile(srcpath, self, CfsOpenOptions.Default) {
                case srcfile : CfsFile ⇒
                    Cfs.withValidPath(dstpath) { dpath ⇒
                        val dbox : Box[(CfsFolder, String)] = {
                            val dpathAbs = dpath match {
                                case abspath : CfsAbsolutePath ⇒ Full(abspath)
                                case relpath : CfsRelativePath ⇒
                                    // If the destination is not an absolute path, it should be just a single
                                    // component, which is the name to be given the file in the same folder
                                    // as the source file. Typically this is used when a file is being renamed
                                    // in place, but it could be used just to make another link to the file.
                                    // In any case, the destination folder is the folder of the source file.
                                    if (relpath.allParts.length == 1) Full(srcfile.getPath.getParent)
                                    else Failure(s"destination path $dstpath should be a full path or a simple name")
                                case _ ⇒ Failure(s"unexpected path type for '$dstpath'")
                            }
                            dpathAbs flatMap { dabs ⇒
                                val trydabs = Cfs.openFolder (dabs, self) { vfile ⇒
                                    vfile close ()
                                    Failure(s"$dstpath does not exist, and $dabs is not a folder")
                                } map ((_, srcfile.getName))
                                trydabs ifempty {
                                    Cfs.openFolder (dabs.getParent, self) { vfile ⇒
                                        vfile close ()
                                        Failure(s"$dstpath does not exist, and $dabs is not a folder")
                                    } map ((_, dabs.getFileName.toString))
                                } ?~ s"neither $dstpath nor $dabs exist"
                            }
                        }
                        // Determine the path of the parent folder of the destination path. If the
                        // destination path is absolute, it is just the parent of that path.
                        // Otherwise it is expected that the destination path is a single name
                        // component, specifying a name to be created in the same folder as the
                        // source file.
                        dbox flatMap { pair ⇒
                            val (dfolder, dname) = pair
                            val result = dfolder link(dname, srcfile) match {
                                case Full(dnew : CfsFile) ⇒
                                    val newpath = dnew.getPath.toString
                                    dnew close()
                                    val renameStatus =
                                        if (rename) {
                                            Cfs.withExistingFile(srcfile.getPath.getParent, self) {
                                                case srcfolder : CfsFolder ⇒
                                                    srcfolder unlink (srcfile.getName, recursive = true) match {
                                                        case Full(_) ⇒ Full(newpath)
                                                        case _ : EmptyBox ⇒
                                                            Failure(s"link succeeded but unlink failed")
                                                    }
                                            }
                                        }
                                        else Full(newpath)
                                    renameStatus map (p ⇒ Map("status" → 1, "path" → p))
                                case Full(other) ⇒
                                    other close ()
                                    dfolder unlink (dname, recursive = true)
                                    Failure(s"link resulted in unexpected file type")
                                case e : EmptyBox ⇒ e ?~ s"link result was Empty"
                            }
                            dfolder close ()
                            result
                        }
                    }
            } flatMap MapResponse
        }
    }

    /**
      * Ajax request to write a string to a file. The file path can be specified explicitly
      * using the "path" argument, or by specifying both "name" and "todir". If only "name"
      * is present, "todir" defaults to a user "Temp" folder, which is created if it doesn't
      * exist. In all other cases, the folder containing the file must already exist.
      *
      * Specifying "mimetype" is optional. It defaults to the MIME type of the output file,
      * if the file already exists. If "mimetype" is specified, it is used as the MIME type
      * of the output file if the file doesn't already exist. Otherwise it must match the
      * MIME type of the existing file. If the files doesn't already exist, and "mimetype"
      * is not specified, it defaults to "text/plain".
      *
      * The "append" option specifies whether the data should be appended to the output
      * file, if the file already exists. It supersedes any value of "replace".
      *
      * The "replace" option specifies that the data should completely replace the data
      * in the output file, if it already exists.
      *
      * If neither "append" nor "replace" are true, the data overwrites data in an existing
      * file, starting from the beginning. That is, if the specified data is shorter than
      * the contents of the file, the contents of the file beyond the length of the "data"
      * is untouched.
      *
      * NOTE: Currently this operation requires that the user is logged in, in addition to
      * having write access to the output file. It could easily be modified to allow guests
      * to write to a file for which guest has write access.
      *
      * @param path the full path of the output file, which supersedes any values for
      *             todir and name which might be present
      * @param name unless path is specified, the filename to write. This must not include
      *             the folder path.
      * @param todir unless path is specified, this is the path to the folder to contain
      *              the output file. A temporary file folder is created if only name
      *              is specified.
      * @param mimetype the file MIME type
      * @param data the data (string) to be written to the file
      * @param replace replace the file if it exists, unless append is also true
      * @param append append to any existing file, else create a new one
      */
    sealed case class AjaxSaveString(path : Option[String], name : Option[String], todir : Option[String],
                                     mimetype : Option[String], data : String,
                                     replace : Option[Boolean], append : Option[Boolean])
        extends AjaxApiRequest("file", "save") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val appendValue = this.append getOrElse false
                val replaceValue = this.replace getOrElse false
                val openOptions = List(
                    List(StandardOpenOption.CREATE, StandardOpenOption.WRITE),
                    // append supersedes replace
                    if (appendValue) List(StandardOpenOption.APPEND)
                    else if (replaceValue) List(StandardOpenOption.TRUNCATE_EXISTING)
                    else Nil,
                    (mimetype map MIME_TYPE).toList
                ).flatten
                try {
                    (path, name, todir) match {
                        case (Some(pathstr), _, _) ⇒
                            Cfs.withValidPath(pathstr) {
                                case abspath : CfsAbsolutePath ⇒
                                    val writer = CfsFiles.newBufferedWriter(abspath, openOptions : _*)(() ⇒ self)
                                    try {
                                        writer write data
                                        SuccessResponse
                                    }
                                    finally {
                                        writer close()
                                    }
                                case _ ⇒
                                    Failure(s"$path is not an absolute path")
                            }
                        case (None, Some(namestr), todirOpt) ⇒
                            if (Cfs isValidName_? namestr) {
                                getTargetFolder(self, namestr, Some(namestr), todirOpt) match {
                                    case Full((tname, tfolder)) ⇒
                                        try {
                                            val outpath = tfolder.getPath / tname
                                            val writer = CfsFiles.newBufferedWriter(outpath, openOptions : _*)(() ⇒ self)
                                            try {
                                                writer write data
                                                SuccessResponse
                                            }
                                            finally {
                                                writer close ()
                                            }
                                        }
                                        finally {
                                            tfolder close ()
                                        }
                                    case e : EmptyBox ⇒
                                        Failure((e ?~ "Empty").messageChain)
                                }
                            }
                            else Failure(s"invalid filename: $name")
                        case _ ⇒ Failure("at least path or name must be specified")
                    }
                }
                catch {
                    case ex : Throwable ⇒ Failure(ex.getMessage)
                }
            }
            else Failure("not logged in")
        }
    }

    /**
      * Upload a file to the Cfs filesystem. This includes options to replace an existing file,
      * and to unpack a .zip or .jar archive. If a destination folder is not specified, the file
      * is uploaded to a temporary folder that is specific to the current user.
      *
      * Request parameters:
      *
      *        name the filename associated with the uploaded file. Typically the user may
      *             not have control over this, as it is set by the upload mechanism to the
      *             name of the source file. This name is used when the toname argument is not
      *             present.
      *      toname this is the optional preferred filename of the target file, which
      *             overrides the name argument.
      *       todir this is the path to the target folder in the filesystem, which must exist
      *             if this argument is specified. If todir is not specified a user-specific
      *             temporary folder is used.
      *    mimetype the optional mimetype of the uploaded file
      *     replace an optional string specifying a replace option for any existing file.
      *             If not specified, the default is "never", except when the file is being
      *             written to a temporary folder, i.e. when todir is not specified, in
      *             which case the default is "always".
      *      lmtime the optional last modified time of the uploaded file. If the replace
      *             argument is "older" and lmtime is not present, the replace option will
      *             be taken as "never". If the replace option is not "older", the lmtime
      *             need not be present.
      *      unpack an optional boolean to unpack an upload .zip or .jar file.  When
      *             replace is specified with unpack, it applies to the individual unpacked
      *             files, not to the folder where they're being unpacked. Any paths in
      *             the archive file are taken as relative to the target folder. The
      *             target filename is not used when unpack is specified.
      *
      * @param req the current request
      */
    sealed case class AjaxLoadFile(req : Req) extends AjaxRequest {

        /**
          * Return a response to the upload request.
          *
          * @param req the request
          * @param sclient the session
          * @param self the current user
          * @return the response
          */
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            // User must be logged in
            if (sclient.loggedIn_?) {
                if (req.request.multipartContent_?) {
                    // Only the first file is used
                    req.uploadedFiles.headOption match {
                        case Some(fph) ⇒ extractRequest(req, fph, self : Principal)
                        case None ⇒ Failure("no file data found")
                    }
                }
                else {
                    Failure("not multipart")
                }
            }
            else Failure("not logged in")
        }

        /**
          * Extract the parameters of the upload request.
          *
          * @param req the request
          * @param fph the uploaded file handle
          * @param self the current user
          * @return the response
          */
        def extractRequest(req : Req, fph : FileParamHolder, self : Principal) : Box[LiftResponse] = {

            // The file input stream needs to be buffered
            val bufferedFileStream = new BufferedInputStream(fph.fileStream)
            val fname = fph.fileName
            // If the MIME type is specified as an explicit parameter, use that
            val mimeType : String = req.param("mimetype") openOr {
                // Otherwise the upload file handle may have a MIME type, which will be our second
                // choice, unless it's something very generic.
                Log.info(s"mimetype not set in load request, file mimetype is ${fph.mimeType}")
                fph.mimeType match {
                    case null | "application/octet-stream" | "binary/octet-stream" ⇒
                        // Try for something less generic
                        val mt = MimeTypeDetector.getMimeType(bufferedFileStream, Some(fname))
                        mt
                    case _ ⇒ fph.mimeType
                }
            }
            val toname = req.param("toname").toOption
            val todir = req.param("todir").toOption
            val replace = req.param("replace").toOption
            val lmtime = req.param("lmtime").flatMap(s ⇒ tryo(s.toLong)).toOption
            val unpack = req.param("unpack").map { uv ⇒ uv != "0" && uv != "false" }.toOption
            upload(self, bufferedFileStream, fname, toname, todir, mimeType, replace, lmtime, unpack)
        }

        /**
          * Do the upload operation. There are two basic types: a single file, and an archive of
          * multiple files which is to be unpacked.
          *
          * @param self the current user
          * @param fileInputStream an input stream for the uploaded file
          * @param name the filename from the upload file handle
          * @param toname the optional target filename
          * @param todir the optional target folder
          * @param mimetype the MIME type
          * @param replace the optional replacement policy
          * @param lmtime the optional last modified time
          * @param unpackOpt the optional unpack parameter, as a boolean
          * @return the response
          */
        def upload(self : Principal, fileInputStream : InputStream, name : String, toname : Option[String],
                   todir : Option[String],
                   mimetype : String, replace : Option[String],
                   lmtime : Option[Long], unpackOpt : Option[Boolean]) : Box[LiftResponse] = {
            // An existing file is replaced if replace is true, or if no todir value
            // was specified, which indicates a temporary file
            val replaceOption = if (todir.isEmpty) ReplaceAlways else ReplaceOption(replace, lmtime)
            val unpack = unpackOpt getOrElse false
            getTargetFolder(self, name, toname, todir) flatMap {
                case (tname, tfolder) ⇒
                    val result = {
                        if (unpack) {
                            val maplist = uploadArchive(fileInputStream, mimetype, tfolder, replaceOption)
                            maplist flatMap (rlist ⇒ MapResponse(Map("status" → 1, "result" → rlist)))
                        }
                        else {
                            val map = uploadFile(fileInputStream, tname, tfolder, mimetype, replaceOption)
                            map flatMap (map ⇒ MapResponse(Map("status" → 1, "result" → map)))
                        }
                    }
                    tfolder close ()
                    result
            }
        }

        def uploadArchive(archiveStream : InputStream, mimetype : String,
                          tfolder : CfsFolder, replaceOption : ReplaceOption) : Box[List[Map[String, Any]]] = {
            import java.util.jar.JarInputStream
            import java.util.zip.ZipInputStream

            mimetype match {
                case "application/zip" | "application/x-zip-compressed" |
                     "application/x-zip" | "application/x-compress" |
                     "application/x-compressed" ⇒
                    val zipInFile = tryo(new ZipInputStream(archiveStream)) map (ZipFile(_))
                    zipInFile map { unpackArchive(_, tfolder, replaceOption) }
                case "application/java-archive" | "application/x-java-archive" ⇒
                    val jarInFile = tryo(new JarInputStream(archiveStream, false)) map (JarFile(_))
                    jarInFile flatMap { jf ⇒ Full(unpackArchive(jf, tfolder, replaceOption)) }
                case other ⇒ Failure(s"unpacking a file of type $other is not supported")
            }
        }

        def uploadFile(fileInputStream : InputStream, tname : String, tfolder : CfsFolder,
                       mimetype : String, replaceOption : ReplaceOption) : Box[Map[String, Any]] = {
            tfolder getMember tname match {
                case Full(tfile : CfsFolder) ⇒
                    tfile close ()
                    Failure(s"$tname already exists as a folder, and cannot be replaced")
                case Full(tfile : CfsPlain) ⇒
                    if (replaceOption.replace_?(tfile)) {
                        replaceFile(fileInputStream, mimetype, tfolder, tfile)
                    }
                    else {
                        val result = tfile.asMap ++ Map("status" → 1, "replaced" → false)
                        tfile close ()
                        Full(result)
                    }
                case Full(jfile : CfsJarFile) ⇒
                    if (replaceOption.replace_?(jfile)) {
                        replaceFile(fileInputStream, mimetype, tfolder, jfile)
                    }
                    else {
                        val result = jfile.asMap ++ Map("status" → 1, "replaced" → false)
                        jfile close ()
                        Full(result)
                    }
                case Full(vfile) ⇒
                    vfile close ()
                    Failure(s"$tname already exists as an irreplaceable type of file")
                case Empty ⇒ createFile(fileInputStream, mimetype, tfolder, tname)
                case f : Failure ⇒
                    Failure(s"error checking existence of $tname", Empty, f)
            }
        }

        def createFile(instream : InputStream, mimetype : String,
                       tfolder : CfsFolder, tname : String) : Box[Map[String, Any]] = {
            val principal = tfolder.getPrincipal
            implicit val principalImpl : () ⇒ Principal = () ⇒ principal
            val mtbox = DbManager getMimeTypeHandler mimetype flatMap { mth ⇒
                if (mth.isSpecial_?) {
                    Failure(s"cannot upload files with MIME type $mimetype")
                }
                else Full(true)
            }
            val pathBox = mtbox flatMap { _ ⇒
                Cfs.withValidFilename(tname) { name ⇒
                    val outpath = tfolder.getPath / name
                    try {
                        val length = CfsFiles.copy(instream, outpath, MIME_TYPE(mimetype))
                        Full(outpath)
                    }
                    catch {
                        case ex : Throwable ⇒
                            Failure(s"Failed to create ${outpath.toString}: ${ex.getMessage}")
                    }
                }
            }
            pathBox flatMap { outpath ⇒
                Cfs.withExistingFile(outpath, tfolder.getPrincipal, CfsOpenOptions.Default) {
                    case cfsfile ⇒ Full(cfsfile.asMap + "status" → 1)
                }
            }
        }

        def createFile(tfolder : CfsFolder, dataSeqnum : Long, tname : String,
                       mimetype : String, modtime : Long) : Box[CfsFile] = {
            val options = CfsCreateOptions(dataSeqnum = dataSeqnum, ctime = modtime)
            tfolder create (tname, mimetype, options)
        }

        /**
          * Replace an existing file with data from an InputStream. This is valid only
          * for plain files or script files. The specified MIME type does not have to
          * match the MIME type of the existing plain file, but it does have to match
          * if the existing file is a supported script type.
          *
          * The modified time of the output file is set to the current time.
          *
          * @param in the input data to replace the existing file
          * @param mimetype the MIME type of the input data, and the resulting MIME
          *                 type of the output file if the operation succeeds
          * @param tfolder the folder containing the output file
          * @param curFile the file to be replaced
          * @return a boxed map of file information for the resulting file
          */
        def replaceFile(in : InputStream, mimetype : String,
                        tfolder : CfsFolder, curFile : CfsFile) : Box[Map[String, Any]] = {
            replaceFileAlt (in, mimetype, tfolder, curFile, None) match {
                case Full(newfile) ⇒
                    val map = newfile.asMap + "status" → 1 + "replaced" → true
                    newfile close ()
                    Full(map)
                case e : EmptyBox ⇒
                    curFile close ()
                    e
            }
        }

        def unpackArchive[T <: ZipEntry](inArchive : ArchiveFile[T],
                             tfolder : CfsFolder, replace : ReplaceOption) : List[Map[String, Any]] = {
            def extractMember(members : Iterator[ArchiveMember],
                              folderCache : Option[(String, CfsFolder)],
                              acc : List[Map[String, Any]]) : List[Map[String, Any]] = {
                if (members.hasNext) {
                    val member = members.next()
                    val mimetype = member.getMimeType
                    // Get the folder in which the member is to be extracted. If the member has
                    // a directory path, ensure that it is a valid Cfs path, and that it is
                    // taken relative to the target folder.
                    val (mfolder, nextFolderCache) =
                        if (member.dirpath == "") (Full(tfolder), folderCache)
                        else folderCache match {
                            case Some((dpath, dfolder)) if dpath == member.dirpath ⇒ (Full(dfolder), folderCache)
                            case _ ⇒
                                Cfs.withValidPath(member.dirpath) { mpath ⇒
                                    val mrelpath = mpath match {
                                        case rel : CfsRelativePath ⇒ rel
                                        case abs ⇒ CfsRelativePath(abs.allParts)
                                    }
                                    // Create the target folder if it does not exist
                                    getOrMakeFolder(tfolder.getPath resolve mrelpath, tfolder.getPrincipal,
                                                    recursive = true, member.modtime)
                                } match {
                                    case Full(dfolder) ⇒
                                        folderCache foreach (_._2 close ())
                                        (Full(dfolder), Some((member.dirpath, dfolder)))
                                    case e : EmptyBox ⇒ (e, folderCache)
                                }
                        }
                    // Extract the member, replacing an existing file if indicated
                    val newfile : Box[(CfsFile, Option[Boolean])] = mfolder flatMap { mtfolder ⇒
                        mtfolder getMember member.name match {
                            case Full(cfsfile : CfsFile) ⇒
                                def replaceMember() : Box[(CfsFile, Option[Boolean])] = {
                                    replaceFileAlt(member.getInputStream, mimetype,
                                                   mtfolder, cfsfile, Some(member.modtime)) map ((_, Some(true)))
                                }
                                replace match {
                                    case ReplaceAlways ⇒ replaceMember()
                                    case _ : ReplaceOlder if ReplaceOlder(member.modtime).replace_?(cfsfile) ⇒
                                        // The matched ReplaceOlder instance has an associated time, but it
                                        // is the member's modification time that matters here
                                        replaceMember()
                                    case _ ⇒ Full((cfsfile, Some(false)))
                                }
                            case Full(vfile) ⇒
                                vfile close ()
                                Failure(s"${member.fullPath} already exists as an irreplaceable file")
                            case Empty ⇒
                                val (dataSeqnum, _) = ChoiceData makeDataFile member.getInputStream
                                createFile(mtfolder, dataSeqnum, member.name,
                                           mimetype, member.modtime) map ((_, None))
                            case f : Failure ⇒ f
                        }
                    }
                    // Make a map describing the extracted member or an error
                    val map =
                        newfile match {
                            case Full((mfile, replaced)) ⇒
                                val mmap = mfile.asMap ++ Map("status" → 1, "replaced" → replaced)
                                mfile close ()
                                mmap
                            case e : EmptyBox ⇒
                                val msg = (e ?~ "unexpectedly got Empty").msg
                                Map[String, Any]("status" → -1,
                                                 "msg" → msg,
                                                 "path" → member.fullPath,
                                                 "mimetype" → mimetype)
                        }
                    extractMember(members, nextFolderCache, map :: acc)
                }
                else {
                    folderCache foreach (_._2 close ())
                    acc
                }
            }
            extractMember(inArchive.filter(!_.isdir), None, Nil)
        }
    }

    sealed case class AjaxCopyFile(path : String, recursive : Option[Boolean],
                                   withAttributes : Option[Boolean], withPolicies : Option[Boolean])
        extends AjaxApiRequest("file", "copy") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val cfsfilelib = new CfsFileLib(() ⇒ self)

            Failure("not yet implemented")
        }
    }

    sealed case class AjaxCleanupFiles(what : String, doit : Option[Boolean]) extends AjaxApiRequest("file", "cleanup") {
        import CfsCleanup._
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (self.isSystemAdmin_?) {
                val takeAction = doit getOrElse false
                val resultmap = what.toLowerCase match {
                    case "choicedata-orphans" ⇒
                        choiceDataOrphans(takeAction = takeAction, self) map {
                            case (orphans, dupes) ⇒ Map("orphans" → orphans, "duprefs" → dupes)
                        }
                    case "choicedata-missing" ⇒
                        choiceDataMissing(takeAction = takeAction, self) map {
                            case (dnpairs, repairs) ⇒ Map("datanode-refs" → dnpairs, "resource-refs" → repairs)
                        }
                    case "datanode-refcounts" ⇒
                        datanodeRefcounts(takeAction = takeAction, self) map { list ⇒
                            Map("refcounts" → list)
                        }
                    case "resource-orphans" ⇒
                        resourceOrphans(takeAction = takeAction, self) map { list ⇒
                            Map("resources" → (list map (_.id)))
                        }
                    case "resource-refcounts" ⇒
                        resourceRefcounts(takeAction = takeAction, self) map { list ⇒
                            Map("refcounts" → list)
                        }
                    case _ ⇒
                        Failure(s"don't know how to clean $what")
                }
                resultmap flatMap (rmap ⇒ MapResponse(rmap))
            }
            else Failure("cleanup requires a system administrator")
        }

    }

    sealed case class AjaxGatherJson(path : String, recursive : Option[Boolean], since : Option[Long])
        extends AjaxApiRequest("file", "gather") {
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val start = millis
            Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                case folder : CfsFolder ⇒ Full(buildJsonArray(folder, self))
            } flatMap { jsarray ⇒ MapResponse(Map("status" → 1, "timestamp" → start, "result" → jsarray)) }
        }

        def buildJsonArray(folder : CfsFolder, by : Principal) : JArray = {
            val members = (folder.getPlainFiles foldLeft (Nil : List[CfsPlain])) { (list, vfile) ⇒
                vfile match {
                    case pfile : CfsPlain ⇒
                        if (pfile isType_? "application/json") pfile :: list
                        else {
                            pfile close ()
                            list
                        }
                    case _ ⇒
                        vfile close ()
                        list
                }
            }
            val jolist = (members foldLeft (Nil : List[JValue])) { (list: List[JValue], pfile: CfsPlain) ⇒
                val include = since match {
                    case Some(stime) ⇒ (pfile.info().toOption fold true)(_.mtime > stime)
                    case None ⇒ true
                }
                val name = pfile.getName
                val result =
                    if (include) {
                        tryo(CfsFiles.newInputStream(pfile)) flatMap { strin ⇒
                            tryo {
                                new InputStreamReader(strin)
                            } flatMap { rdr ⇒ JsonParser.parseOpt(rdr, closeAutomatically = true)}
                        }
                    }
                    else {
                        pfile close()
                        Empty
                    }
                result match {
                    case Full(jv) ⇒ decompose(Map("name" → name, "data" → jv)) :: list
                    case _ : EmptyBox ⇒ list
                }
            }
            val fullList =
                if (recursive getOrElse false) {
                    val folders = folder.getFolders
                    (folders foldLeft jolist) { (list, subfolder) ⇒
                        val fname = subfolder.getName
                        val result = {
                            val subja = buildJsonArray(subfolder, by)
                            if (subja.values.nonEmpty) {
                                val mjson = decompose(Map("name" → fname, "members" → subja))
                                Full(mjson)
                            }
                            else Empty
                        }
                        subfolder close ()
                        result match {
                            case Full(jo) ⇒ jo :: list
                            case _ : EmptyBox ⇒ list
                        }
                    }
                }
                else jolist
            JArray(fullList)
        }
    }

    sealed case class AjaxRemoveFile(path : String, recursive : Option[Boolean])
        extends AjaxApiRequest("file", "rm") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withValidPath(path) { fpath ⇒
                val ppath = fpath.getParent
                Cfs.withExistingFile(ppath, self) {
                    case pfile : CfsDirFile ⇒
                        pfile unlink (fpath.getFileName.toString, recursive getOrElse false) flatMap { bool ⇒
                            Full(Map("status" → 1, "result" → bool))
                        }
                }
            } flatMap MapResponse
        }
    }

    sealed case class AjaxDefineAttribute(path : String, atype : String, description : Option[String])
        extends AjaxApiRequest("file", "adefn") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val pairbox = AttributeType convertAttributeTypeString atype flatMap { atypeval ⇒
                AttrDef getAttributePath path flatMap { apath ⇒
                    AttrDef openDefinition (apath, self, CfsOpenOptions.Default) match {
                        case Full(adef) ⇒
                            val result = adef.getData flatMap { ainfo ⇒
                                if (ainfo.atype == atypeval) Full((0, adef.asMap))
                                else Failure(s"attribute already exists with type ${ainfo.atype}")
                            }
                            adef close ()
                            result
                        case Empty ⇒
                            val options = AttrDefCreateOptions(atype = atypeval, description = description)
                            Cfs create (apath, self, AttrDef.getMimeType, options) match {
                                case Full(adef : AttrDef) ⇒
                                    val result = adef.getData map { _ ⇒ (1, adef.asMap) }
                                    adef close ()
                                    result
                                case Full(other) ⇒
                                    other close ()
                                    Failure(s"system error: wrong file type")
                                case e : EmptyBox ⇒ e ?~! s"failed to create attribute file: $apath"
                            }
                        case f : Failure ⇒ f ?~! s"$path may already exist but is not accessible"
                    }
                }
            }
            pairbox flatMap {
                case (status, amap) ⇒ MapResponse(Map("status" → status) ++ amap)
            }
        }
    }

    /**
     * Get all of the value assignments for the attribute defined by the file at 'path'. An
     * entry is returned for each value assignment with information about the file to which
     * the value is assigned, and the value itself. However, some of these entries may
     * indicate errors, most likely due to a lack of access rights for the current user.
     *
     * @param path the file path to the attribute definition
     */
    sealed case class AjaxGetValuesOfAttribute(path : String) extends AjaxApiRequest("file", "avoa") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            AttrDef getAttributePath path flatMap { apath ⇒
                Cfs.withExistingFile (apath, self) {
                    case adef : AttrDef ⇒
                        val attId = AttributeId(adef.getResourceId.id)
                        adef.getValues match {
                            case Nil ⇒ SimpleResponse(0, "no values")
                            case list ⇒
                                val maplist = list map { aval ⇒
                                    val targetId = aval.target.get
                                    Cfs open (CfsVFileId(targetId), self, CfsOpenOptions.Default) match {
                                        case Full(cfsfile : CfsFile) ⇒
                                            val filemap = cfsfile.asMap
                                            val result = cfsfile getAttribute (attId, None) match {
                                                case Full(ainfo) ⇒
                                                    Map("status" → 1, "file" → filemap, "value" → ainfo.asMap)
                                                case e : EmptyBox ⇒
                                                    Map("status" → -1, "file" → filemap, "msg" → (e ?~ "Empty").msg)
                                            }
                                            cfsfile close ()
                                            result
                                        case Full(other) ⇒
                                            val path = other.getPath
                                            other close ()
                                            Map[String, Any]("status" → -1,
                                                "file" → Map("path" → path),
                                                "msg" → s"system error: non-Cfs file $path has attribute value")
                                        case e : EmptyBox ⇒
                                            Map[String, Any]("status" → -1,
                                                "file" → Map("id" → targetId),
                                                "msg" → (e ?~ "Empty").msg)
                                    }
                                }
                                MapResponse(Map("status" → maplist.length, "path" → apath.toString, "values" → maplist))
                        }
                }
            }
        }
    }

    sealed case class AjaxGetAttributeValues(path : String, attributes : Option[List[String]])
        extends AjaxApiRequest("file", "aget") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile (path, self, CfsOpenOptions.Default) {
                case cfsfile : CfsFile ⇒
                    val attrList = attributes match {
                        case Some(list) ⇒ list map { name ⇒
                            AttrDef getAttributeId name match {
                                case Full(id) ⇒ (Full(name), Full(id))
                                case e : EmptyBox ⇒ (Full(name), e)
                            }
                        }
                        case None ⇒
                            def helper(idlist : List[AttributeId],
                                       acc : List[(Box[String], Box[AttributeId])]) : List[(Box[String], Box[AttributeId])] = {
                                idlist match {
                                    case Nil ⇒ acc
                                    case head :: tail ⇒
                                        val id = head.id
                                        Cfs open (CfsVFileId(id), self, CfsOpenOptions.Default) match {
                                            case Full(adef : AttrDef) ⇒
                                                val path = adef.getPath.toString
                                                adef close ()
                                                helper (tail, (Full(path), Full(head)) :: acc)
                                            case Full(other) ⇒
                                                other close ()
                                                Log.error(s"system error: $id is not an attribute id")
                                                helper (tail, acc)
                                            case Empty ⇒
                                                val pair = (Failure(s"missing attribute definition for attribute id $id"), Full(head))
                                                helper (tail, pair :: acc)
                                            case _ : Failure ⇒ helper (tail, acc)
                                        }
                                }
                            }
                            val idlist = AttrVal getAll cfsfile.getResourceId map { aval ⇒
                                AttributeId(aval.adef.get)
                            }
                            helper (idlist, Nil)
                    }
                    val resultList = attrList map {
                        case (_, Full(id)) ⇒ cfsfile getAttribute (id, None)
                        case (_, e : EmptyBox) ⇒ e
                    }
                    val resultMaps = attrList zip resultList map {
                        case ((Full(name), _), Full(ainfo)) ⇒
                            Map("status" → 1, "name" → name) ++ ainfo.asMap
                        case ((Full(name), _), e : EmptyBox) ⇒
                            Map[String, Any]("status" → -1, "name" → name, "msg" → (e ?~ "Empty").msg)
                        case ((e : EmptyBox, _), Full(ainfo)) ⇒
                            Map[String, Any]("status" → -1, "msg" → (e ?~ "Empty").msg) ++ ainfo.asMap
                        case (_, e : EmptyBox) ⇒
                            Map[String, Any]("status" → -1, "msg" → (e ?~ "Empty").msg)
                    }
                    MapResponse(Map("status" → 1, "attributes" → resultMaps))
            }
        }
    }

    case class NVPair(name : String, value : JValue, atype : Option[String])

    sealed case class AjaxSetAttributeValues(path : String, attributes : List[NVPair])
        extends AjaxApiRequest("file", "aset") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile (path, self, CfsOpenOptions.Default) {
                case cfsfile : CfsFile ⇒
                    val resultList = attributes map {
                        case NVPair(name, value, atype) ⇒
                            val atoptbox = atype map AttributeType.convertAttributeTypeString
                            atoptbox match {
                                case None | Some(Full(_)) ⇒
                                    val atypeval = atoptbox map (_ openOr AttributeType.STRING)
                                    AttrDef getAttributeId name flatMap { id ⇒
                                        cfsfile setAttribute (id, atypeval, value)
                                    }
                                case Some(e : EmptyBox) ⇒ e
                            }
                    }
                    val resultMaps = attributes zip resultList map {
                        case (nvpair, Full(ainfo)) ⇒
                            Map("status" → 1, "name" → nvpair.name) ++ ainfo.asMap
                        case (nvpair, e : EmptyBox) ⇒
                            Map[String, Any]("status" → -1, "name" → nvpair.name, "msg" → (e ?~ "Empty").msg)
                    }
                    MapResponse(Map("status" → 1, "attributes" → resultMaps))
            }
        }
    }

    /**
     * Copy attribute value assignments from one file to another. By default, any existing values
     * for the same attributes on the target file are replaced. However, the dupmode option
     * allows the caller to prefer the value already assigned to the target, or to prefer the
     * newer of the source and target values.
     *
     * @param from file path of source file
     * @param to file path of target file
     * @param dupmode default value of 0 prefers source file values, value of 1 prefers
     *                target file values, 2 prefers newest of source and target values
     */
    sealed case class AjaxCopyAttributeValues(from : String, to : String, dupmode : Option[Int])
        extends AjaxApiRequest("file", "acopy") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val dupcode = dupmode getOrElse 0
            if ((dupcode < 0) || (dupcode > 2)) Failure(s"invalid dupmode $dupcode")
            else {
                Cfs.withExistingFile (from, self, CfsOpenOptions.Default) {
                    case source ⇒ Cfs.withExistingFile (to, self, CfsOpenOptions.Default) {
                        case target ⇒
                            val srclist = AttrVal getAll source.getResourceId
                            val dstlist = AttrVal getAll target.getResourceId
                            def helper(list : List[AttrVal], acc : List[AttributeId]) : List[AttributeId] = {
                                list match {
                                    case Nil ⇒ acc
                                    case head :: tail ⇒
                                        dstlist find (_.adef.get == head.adef.get) match {
                                            case Some(dval) ⇒
                                                dupcode match {
                                                    case 1 ⇒
                                                        // target value prevails
                                                        helper (tail, acc)
                                                    case 2 ⇒
                                                        // newer value prevails (source if tie)
                                                        if (head.stime.get >= dval.stime.get) {
                                                            helper (tail, AttributeId(head.adef.get) :: acc)
                                                        }
                                                        else helper (tail, acc)
                                                    case _ ⇒
                                                        // target doesn't have this value
                                                        helper (tail, AttributeId(head.adef.get) :: acc)
                                                }
                                            case None ⇒ helper (tail, AttributeId(head.adef.get) :: acc)
                                        }
                                }
                            }
                            val idlist = helper (srclist, Nil)
                            tryo {
                                idlist foreach { id ⇒
                                    // Ensure the current user can read the source attribute
                                    source getAttribute (id, None) foreach { _ ⇒
                                        // And can write the target attribute (clearing it)
                                        target setAttribute (id, None, null) foreach { _ ⇒
                                            // Then copy the AttrVal to the target
                                            AttrVal get (id, source.getResourceId) foreach { srcval ⇒
                                                tryo {
                                                    AttrVal.create.target(target.getResourceId.id)
                                                        .adef(srcval.adef.get).atype(srcval.atype.get)
                                                        .value(srcval.value.get).setter(srcval.setter.get)
                                                        .stime(srcval.stime.get).saveMe()
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            val maplist = idlist map { id ⇒
                                target getAttribute (id, None) match {
                                    case Full(tainfo) ⇒ tainfo.asMap
                                    case e : EmptyBox ⇒ Map[String, Any]("id" → id.id, "msg" → (e ?~ "Empty").msg)
                                }
                            }
                            val tpath = target.getPath.toString
                            MapResponse(Map("status" → 1, "file" → tpath, "attributes" → maplist))
                    }
                }
            }
        }
    }

    sealed case class AjaxMakeJar(outpath : String, inpath : String, fentry : Option[Boolean])
        extends AjaxApiRequest("file", "mkjar") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(inpath) { pin ⇒
                    Cfs.withValidPath(outpath) { pout ⇒
                        CfsJarFile(pin, pout, self, fentry getOrElse false) flatMap { dbjar ⇒
                            val map = dbjar.asMap + ("name" → dbjar.getName) + ("status" → 1)
                            dbjar close ()
                            MapResponse(map)
                        }
                    }
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxMakeLib(libpath : String) extends AjaxApiRequest("file", "mklib") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(libpath) { lpath ⇒
                    Library.create(lpath, self) match {
                        case Full(lib : Library) ⇒
                            val map = lib.asMap + ("name" → lib.getName) + ("status" → 1)
                            lib close ()
                            MapResponse(map)
                        case Empty ⇒ Failure("failed to create library")
                        case f : Failure ⇒ f
                    }
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxPublish(libpath : String, groupId : String, artifactId : String,
                                  version : String, path : String)
        extends AjaxApiRequest("file", "publish") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile(libpath, self, CfsOpenOptions.Default) {
                case library : Library ⇒
                    Cfs.withValidPath(path) { cfspath ⇒
                        library.publish(groupId, artifactId, version, cfspath) match {
                            case Full(_) ⇒ Full("ok")
                            case e : EmptyBox ⇒ e
                        }
                    }
                case dbfile : CfsFile ⇒ Failure(s"${dbfile.getPath.toString} is not a library")
                case _ ⇒ Failure("operation not supported for non-Cfs file")
            } flatMap (_ ⇒ SuccessResponse)
        }
    }

    /**
      * Create a CSV file from data in JSON-encoded arrays.
      *
      * @param headers an array of strings that are the column headers
      * @param data an array of arrays, each containing a row of data
      * @param name the name of the CSV file to be created, default "data.csv"
      * @param path the path to the folder where the CSV file will be stored,
      * 			   default to a temp folder
      * @param replace false if an existing file should not be replaced
      *                  default to true, replace existing file
     */
    sealed case class AjaxMakeCsv(headers : JArray, data : JArray,
                                  name : Option[String], path : Option[String],
                                  replace : Option[Boolean])
        extends AjaxApiRequest("file", "mkcsv") {

        def fieldToString(jv : JValue) : String = {
            jv match {
                case JInt(i) ⇒ i.toString()
                case JString(s) ⇒
                    "\"" + s.replaceAll("\n", " ").replaceAll("^=", " ").replaceAll("\"", "\"\"") + "\""
                case JDouble(d) ⇒ d.toString
                case _ ⇒ "?"
            }
        }
        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val hdrlist = headers match {
                    case JArray(list) ⇒ list.map(jv ⇒ fieldToString(jv))
                    case _ ⇒ List("?")
                }
                val table = data match {
                    case JArray(rows) ⇒ rows.map {
                        case JArray(list) ⇒ list.map(jv ⇒ fieldToString(jv))
                        case _ ⇒ List("?")
                    }
                }
                val replace = this.replace getOrElse (path fold true)(_ ⇒ false)
                val all = hdrlist :: table
                val enc = all.map(_.mkString("", ",", "")).mkString("", "\n", "\n")
                val respbytes = augmentString(enc).map(_.toByte).toArray
                val mapbox = getTargetFolder(self, "data.csv", name, path) match {
                    case Full((tname, tfolder)) ⇒
                        val ok = tfolder getMember tname match {
                            case Full(dbcont : CfsFolder) ⇒
                                dbcont close ()
                                Failure(s"cannot replace existing folder $tname")
                            case Full(dbfile : CfsFile) ⇒
                                dbfile close ()
                                if (dbfile.isSpecial_?) Failure(s"special file $tname cannot be replaced")
                                else if (replace) tfolder unlink (tname, recursive = false)
                                else Failure(s"$tname already exists and replace not requested")
                            case Full(vfile) ⇒
                                vfile close ()
                                Failure(s"cannot replace existing non-Cfs file $tname")
                            case Empty ⇒ Full(true)
                            case f : Failure ⇒ f
                        }
                        val result = ok flatMap {
                            _ ⇒
                                val (dataSeqnum, _) = ChoiceData makeDataFile respbytes
                                val options = CfsCreateOptions(replace = true, dataSeqnum = dataSeqnum)
                                tfolder create (tname, "text/csv", options) flatMap {
                                    dbfile : CfsFile ⇒
                                        dbfile close ()
                                        Full(Map[String, Any]("status" → 1,
                                                 "path" → tfolder.getPath.toString,
                                                 "name" → tname, "length" → respbytes.length))
                                }
                        }
                        tfolder close ()
                        result
                    case e : EmptyBox ⇒ e
                }
                mapbox flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    import scala.util.parsing.combinator._
    class CSV(sep : String) extends RegexParsers {
        import scala.language.postfixOps
        override val skipWhitespace = false   // meaningful spaces in CSV

        def COMMA = ","
        def DQUOTE = "\""
        def DQUOTE2 : Parser[String] = "\"\"" ^^ (_ ⇒ "\"")
        def CR = "\r"
        def LF = "\n"
        def CRLF = "\r\n"
        def TXT : Regex = ("[^\"" + sep + "\r\n]").r
        def SPACES : Regex = "[ \t]+".r

        def file : Parser[List[List[String]]] = repsep(record, CRLF) <~ opt(CRLF)
        def record : Parser[List[String]] = rep1sep(field, sep)
        def field : Parser[String] = escaped | nonescaped
        def escaped : Parser[String] = {
            ((SPACES ?) ~> DQUOTE ~> ((TXT | sep | CR | LF | DQUOTE2) *) <~ DQUOTE <~ (SPACES ?)) ^^ (ls ⇒ ls.mkString(""))
        }
        def nonescaped : Parser[String] = (TXT*) ^^ (ls ⇒ ls.mkString(""))

        def parse(s : String) : List[List[String]] = parseAll(file, s) match {
            case Success(res, _) ⇒ res
            case _ ⇒ List[List[String]]()
        }
    }
    
    /**
     * Read an existing CSV file and return it as JSON arrays.
     */
    sealed case class AjaxReadCsv(path : String) extends AjaxApiRequest("file", "rdcsv") {
    	import scala.io.Source

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withValidPath(path) {
                case abspath : CfsAbsolutePath ⇒
                    tryo(CfsFiles.newInputStream(abspath)(() ⇒ self)) flatMap { csvin ⇒
                        val lines = Source.fromInputStream(csvin).getLines().toList
                        csvin close ()
                        val sep = lines.length match {
                            case 0 ⇒ ","
                            case _ ⇒ if (lines.head.split(",").length > lines.head.split(";").length) ","
                            else ";"
                        }
                        val parser = new CSV(sep)
                        val plines = parser.parse(lines.mkString("\r\n")).map(line ⇒ {
                            JArray(line.map(item ⇒ {
                                tryo(item.toInt) match {
                                    case Full(n) ⇒ JInt(n)
                                    case _ ⇒ JString(item)
                                }
                            }))
                        })
                        val fields : List[(String, JValue)] = List("status" → JInt(1),
                            "path" → JString(path),
                            "data" → JArray(plines))
                        Full(JsonResponse(JObject(fields.map(pair ⇒ JField(pair._1, pair._2)))))
                    }
                case other ⇒ Failure(s"${other.toString} is not an absolute path")
            }
        }
    }

    sealed case class AjaxCreatePolicy(path : String, desc : Option[String])
        extends AjaxApiRequest("file", "mkpolicy") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(path) { cfspath ⇒
                    Policy createPolicy (cfspath, self) flatMap { pfile ⇒
                        val policy = desc match {
                            case Some(s) ⇒ pfile describePolicy s match {
                                case Full(pf) ⇒ pf
                                case _ : EmptyBox ⇒
                                    Log.error(s"error setting policy description: $s")
                                    pfile
                            }
                            case None ⇒ pfile
                        }
                        val result = policy.asMap
                        policy close ()
                        MapResponse(result + ("status" → 1))
                    }
                }
            }
            else Failure("not logged in")
        }
    }

    /**
     * Representation of a role assignment in access control operations. A role assignment
     * is an association of a principal with a role.
     *
     * @param principal a path to the principal, typically a user or a group
     * @param role the name of a role (currently all roles are kept in /System/Roles)
     */
    sealed case class RoleAssignment(principal : String, role : String)

    /**
     * Add role assignments to a policy.
     *
     * @param path path to a policy file
     * @param list list of assignments of principals to roles
     */
    sealed case class AjaxAddRole(path : String, list : List[RoleAssignment])
        extends AjaxApiRequest("file", "addrole") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                case policy : Policy ⇒
                    val results : List[Map[String, Any]] = list map {
                        case RoleAssignment(ppath, rolename) ⇒
                            val basemap : Map[String, Any] = Map("principal" → ppath, "role" → rolename)
                            Cfs.withExistingFile[Map[String, Any]](ppath, self, CfsOpenOptions.Default) {
                                case p : IsPrincipal ⇒
                                    val pid = p.getSelfPrincipalId
                                    RoleFile.getRoleId(rolename) match {
                                        case Full(roleId) ⇒
                                            policy addRole List((pid, roleId)) match {
                                                case Full(_) ⇒ Full(basemap + ("status" → 1))
                                                case e : EmptyBox ⇒ Full(basemap ++ Map("status" → -1, "msg" → e.toString))
                                            }
                                        case _ : EmptyBox ⇒
                                            Full(basemap ++ Map("status" → -1, "msg" → "not a role"))
                                    }
                                case _ ⇒ Full(basemap ++ Map("status" → -1, "msg" → "not a principal"))
                            } match {
                                case Full(map) ⇒ map
                                case e : EmptyBox ⇒ basemap ++ Map("status" → -1, "msg" → e.toString)
                            }
                    }
                    ArrayResponse(results)
                case _ : CfsFile ⇒ Failure(s"$path is not a policy file")
            }
        }
    }

    /**
     * Remove role assignments from a policy.
     *
     * @param path path to a policy file
     * @param list a list of assignments of principals to roles
     */
    sealed case class AjaxRmRole(path : String, list : List[RoleAssignment])
        extends AjaxApiRequest("file", "rmrole") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                    case policy : Policy ⇒
                        val results : List[Map[String, Any]] = list map {
                            case RoleAssignment(ppath, rolename) ⇒
                                val basemap : Map[String, Any] = Map("principal" → ppath, "role" → rolename)
                                Cfs.withExistingFile[Map[String, Any]](ppath, self, CfsOpenOptions.Default) {
                                    case p : IsPrincipal ⇒
                                        val pid = p.getSelfPrincipalId
                                        RoleFile.getRoleId(rolename) match {
    //                                    UserRole.values.find(_.toString == rolename.toUpperCase) match {
    //                                        case Some(role) ⇒
                                            case Full(roleId) ⇒
                                                policy removeRole List((pid, roleId)) match {
                                                    case Full(_) ⇒ Full(basemap + ("status" → 1))
                                                    case e : EmptyBox ⇒ Full(basemap ++ Map("status" → -1, "msg" → e.toString))
                                                }
    //                                        case None ⇒
                                            case _ : EmptyBox ⇒
                                                Full(basemap ++ Map("status" → -1, "msg" → "not a role"))
                                        }
                                    case _ ⇒ Full(basemap ++ Map("status" → -1, "msg" → "not a principal"))
                                } match {
                                    case Full(map) ⇒ map
                                    case e : EmptyBox ⇒ basemap ++ Map("status" → -1, "msg" → e.toString)
                                }
                        }
                        ArrayResponse(results)
                    case _ : CfsFile ⇒ Failure(s"$path is not a policy file")
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxListAccess(path : String) extends AjaxApiRequest("file", "lsaccess") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                    case cfsfile : CfsFile ⇒
                        val policyIds = cfsfile getPolicies self
                        val ppaths = policyIds flatMap { pid ⇒
                            Cfs open (CfsVFileId(pid), self, CfsOpenOptions.Default) match {
                                case Full(policy : Policy) ⇒
                                    val pname = policy.getPath.toString
                                    policy close ()
                                    Full(pname)
                                case Full(other) ⇒
                                    other close ()
                                    Log.error(s"Some PolicyRef references ${pid.id} as a policy, but it isn't")
                                    Empty
                                case Empty ⇒
                                    Log.error(s"Some PolicyRef references a non-existent policy (id ${pid.id})")
                                    Empty
                                case f : Failure ⇒
                                    Log.error(s"error accessing policy id ${pid.id}", f)
                                    Empty
                            }
                        }
                        val pathPolicies = cfsfile.getPathPolicies map (_.asMap)
                        Full(Map("status" → 1, "policies" → ppaths, "pathPolicies" → pathPolicies))
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    /**
     * List the role assignments of a policy. The access rights associated with the assigned roles
     * are also returned.
     *
     * @param path path to a policy file
     */
    sealed case class AjaxListPolicy(path : String) extends AjaxApiRequest("file", "lspolicy") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile(path, self, CfsOpenOptions.Default) {
                    case policy : Policy ⇒
                        val list = policy.listRoles flatMap { tuple ⇒
                            val (principal, principalId, roleId) = tuple
                            getRoleRightsMaps(roleId, self) flatMap { pair ⇒
                                val (role, rights) = pair
                                val map = Map(
                                    "principal" → principal,
                                    "principalId" → principalId.id,
                                    "role" → role,
                                    "roleId" → roleId.id,
                                    "rights" → rights
                                )
                                Full(map)
                            }
                        }
                        Full(Map("status" → 1, "list" → list))
                    case _ ⇒ Failure(s"$path is not a policy file")
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxListRole(role : String) extends AjaxApiRequest("file", "lsrole") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                RoleFile getRoleId role flatMap { roleId ⇒
                    getRoleRightsMaps (roleId, self) flatMap { pair ⇒
                        val (_, rights) = pair
                        Full(Map("status" → 1, "role" → role, "roleId" → roleId, "rights" → rights))
                    }
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    def getRoleRightsMaps(roleId : RoleId, principal : Principal) : Box[(String, List[Map[String, Any]])] = {
        (RoleFile getRoleFile roleId) flatMap { rfile ⇒
            val rights = RoleFile getRoleRights roleId map { rdef ⇒
                Map("name" → rdef.name,
                    "applicability" → rdef.applicability,
                    "description" → rdef.description)
            }
            val role = rfile.getName
            rfile close ()
            Full((role, rights))
        }
    }
    
    /**
     * Create a new role. A role is a container for access rights. All system-defined access rights
     * (currently the only kind) are kept in the folder, /System/Rights. Roles can be defined by
     * an administrator, and are essentially collections of access rights kept in /System/Roles.
     *
     * @param name the name of the role, which must be unique
     * @param description a user-friendly description of the role
     * @param rights an optional list of right names to be included in the role
     */
    sealed case class AjaxCreateRole(name : String, description : Option[String], rights : Option[List[String]])
        extends AjaxApiRequest("file", "mkrole") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                RoleFile createRole (name, description getOrElse "", self) flatMap { rolefile ⇒
                    val roleId = rolefile.getRoleId
                    val rlist = rights map { list ⇒
                        list flatMap { rname ⇒
                            rolefile addRight (rname, self) flatMap { rdef ⇒
                                Full(Map("name" → rdef.name,
                                    "applicability" → rdef.applicability,
                                    "description" → rdef.description))
                            }
                        }
                    }
                    rolefile close ()
                    MapResponse(Map("status" → 1, "role" → name, "roleId" → roleId.id, "rights" → rlist))
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxEditRole(role : String, rights : List[String]) extends AjaxApiRequest("file", "edrole") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile(s"${Startup.RolesFolderPath}/$role", self, CfsOpenOptions.Default) {
                    case rolefile : RoleFile ⇒
                        val rdefs = RoleFile getRoleRights rolefile.getRoleId
                        val resultmaps = rights map { rstring ⇒
                            val (rname, remove) = rstring.charAt(0) match {
                                case '-' ⇒ (rstring.substring(1), true)
                                case '+' ⇒ (rstring.substring(1), false)
                                case _ ⇒ (rstring, false)
                            }
                            if (remove) {
                                rdefs find (_.name == rname) match {
                                    case Some(rdef) ⇒
                                        rolefile removeRight rname match {
                                            case Full(_) ⇒
                                                rdef.asMap ++ Map("status" → 1, "result" → "removed")
                                            case e : EmptyBox ⇒
                                                val f = e ?~ "(Empty)"
                                                Map[String, Any]("name" → rname, "status" → -1, "msg" → f.messageChain)
                                        }
                                    case None ⇒ Map[String, Any]("name" → rname, "status" → 0, "result" → "absent")
                                }
                            }
                            else {
                                rdefs find (_.name == rname) match {
                                    case Some(rdef) ⇒ rdef.asMap ++ Map("status" → 0, "result" → "present")
                                    case None ⇒
                                        rolefile addRight (rname, self) match {
                                            case Full(rdef) ⇒
                                                rdef.asMap ++ Map("status" → 1, "result" → "added")
                                            case e : EmptyBox ⇒
                                                val f = e ?~ "(Empty)"
                                                Map[String, Any]("name" → rname, "status" → -1, "msg" → f.messageChain)
                                        }
                                }
                            }
                        }
                        ArrayResponse(resultmaps)
                }
            }
            else Failure("not logged in")
        }
    }

    def processPolicyChangeList(list : List[(String, Box[Boolean])]) : Map[String, Any] = {
        val listmaps = list map {
            case (path, Full(result)) ⇒
                Map[String, Any]("status" → 1, "path" → path, "result" → result)
            case (path, e : EmptyBox) ⇒
                Map[String, Any]("status" → -1, "path" → path, "result" → e)
        }
        val count = (list foldLeft 0) { (n, pair) ⇒
            pair match {
                case (_, Full(true)) ⇒ n + 1
                case _ ⇒ n
            }
        }
        Map("status" → 1, "count" → count, "files" → listmaps)
    }

    /**
     * Apply a specified policy to a set of files identified by a file specification
     * string. The string may be the full path to a single file, or the path of
     * a container file with \/\* or \/\*\* appended. The wildcard suffixes indicate
     * the container and its members, or the container and all its descendants,
     * respectively. No action is taken if the policy is already associated with a
     * file. Otherwise an entry is created in the PolicyRef table to associate the
     * policy with each file.
     *
     * @param policy a path to a policy file
     * @param filespec a specification of the files to which the policy is to be applied.
     */
    sealed case class AjaxShare(policy : String, filespec : String) extends AjaxApiRequest("file", "share") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile (policy, SystemPrincipal, CfsOpenOptions.Default) {
                    case policy : Policy ⇒
                        val list = policy protect (filespec, self)
                        Full(processPolicyChangeList (list))
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    /**
     * Remove the association of a given policy with a specified file. No action
     * is taken if the policy is not associated with the file.
     *
     * @param policy a path to a policy file
     * @param filespec a specification of the files from which the policy is to be removed.
     */
    sealed case class AjaxUnshare(policy : String, filespec : String) extends AjaxApiRequest("file", "unshare") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile (policy, SystemPrincipal, CfsOpenOptions.Default) {
                    case policy : Policy ⇒
                        val list = policy unprotect (filespec, self)
                        Full(processPolicyChangeList (list))
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    /**
     * Apply an access control policy lazily to files, based on their file paths matching a specified
     * pattern, and optionally restricted to files of an indicated MIME type. The path pattern may be
     * specified as either a regular expression or a glob pattern.
     *
     * A policy applied in this way will only be effective when the file path (and optionally MIME type)
     * match, and the user who applied the path policy has the right to set access control on the file.
     * Typically that would mean that the principal who applied the path policy would be an owner of
     * the file or a system administrator.
     *
     * @param policy a path to a policy file
     * @param pattern a regular expression or glob pattern, as indicated by ptype
     * @param ptype "regex" if pattern is a regular expression, "globv1" if it is a glob pattern
     * @param mimetype an optional MIME type that becomes part of the path policy, so that it only
     *                 applies to files of this MIME type
     */
    sealed case class AjaxPShare(policy : String, pattern : String, ptype : String,
                                 mimetype : Option[String]) extends AjaxApiRequest("file", "pshare") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val ptypeVal = ptype.toLowerCase match {
                case "prefix" ⇒ Full(PathPatternType.PREFIX)
                case "regex" ⇒ Full(PathPatternType.REGEX)
                case "globv1" ⇒ Full(PathPatternType.GLOBV1)
                case _ ⇒ Failure(s"invalid path pattern type '$ptype'")
            }
            ptypeVal flatMap { patternType ⇒
                if (sclient.loggedIn_?) {
                    Cfs.withExistingFile (policy, SystemPrincipal, CfsOpenOptions.Default) {
                        case policy : Policy ⇒
                            PathPolicy set (policy.getResourceId, pattern, patternType, mimetype, self) map (_.asMap)
                    } flatMap MapResponse
                }
                else Failure("not logged in")
            }
        }
    }

    /**
     * Remove a path policy with the specified id. The current user must match the principal associated
     * with the path policy (exactly or as a descendant, if the path policy principal is a group), or be
     * a system administrator.
     *
     * The id of a path policy may be obtained from the results of "lsaccess" on a particular file,
     * or by using the "pplist" operation.
     *
     * @param id the id of a path policy
     */
    sealed case class AjaxPUnshare(id : Long) extends AjaxApiRequest("file", "punshare") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                PathPolicy remove (id, self) flatMap { rmret ⇒
                    if (rmret) SuccessResponse
                    else Failure("remove operation failed")
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxPPList() extends AjaxApiRequest("file", "pplist") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                ArrayResponse (PathPolicy list self map (_.asMap))
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxChown(path : String, owner : Option[String]) extends AjaxApiRequest("file", "chown") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(path) { cfspath ⇒
                    Cfs open (cfspath, self, CfsOpenOptions.Default) match {
                        case Full(cfsfile : CfsFile) ⇒
                            val powner = owner match {
                                case Some(ownerpath) ⇒
                                    Cfs.withExistingFile(ownerpath, self, CfsOpenOptions.Default) {
                                        case p : IsPrincipal ⇒ Full(p.getSelfPrincipal)
                                        case _ ⇒ Failure(s"$ownerpath is not a principal")
                                    }
                                case None ⇒ Full(self)
                            }
                            powner match {
                                case Full(p) ⇒
                                    cfsfile chown p match {
                                        case Full(newfile) ⇒
                                            newfile close ()
                                            Full(Map("status" → 1, "path" → path, "owner" → p.getPrincipalName))
                                        case e : EmptyBox ⇒
                                            cfsfile close ()
                                            e
                                    }
                                case e : EmptyBox ⇒
                                    cfsfile close ()
                                    e
                            }
                        case Full(vfile) ⇒
                            vfile close ()
                            Failure("chown not supported on non-Cfs files")
                        case Empty ⇒ Failure(s"$path does not exist")
                        case f : Failure ⇒ f
                    }
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxParseComponent(source : String, output : Option[String])
        extends AjaxApiRequest("file", "pcomp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                val cfsFileLib = new CfsFileLib(() ⇒ self)
                cfsFileLib.compileComponent(source, output) flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxCreateComponent(path : String) extends AjaxApiRequest("file", "mkcomp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withValidPath(path) { cfspath ⇒
                    val ok = Cfs open (cfspath, self, CfsOpenOptions.Default) match {
                        case Full(comp : Component) ⇒
                            Cfs remove (comp, recursive = false) map (_ ⇒ true)
                        case Full(other) ⇒
                            other close ()
                            Failure(s"${cfspath.toString} already exists and is not a component")
                        case Empty ⇒ Full(true)
                        case f : Failure ⇒ f
                    }
                    ok flatMap { _ ⇒
                        Cfs create (cfspath, self, Component.getMimeType, CfsCreateOptions.Default) match {
                            case Full(comp : Component) ⇒
                                val result = comp putData Component.defaultComponentDescriptor
                                comp close ()
                                result flatMap (_ ⇒ SuccessResponse)
                            case Full(other) ⇒
                                other close ()
                                Failure(s"component creation returned wrong file type")
                            case e : EmptyBox ⇒ e
                        }
                    }
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxReadComponent(component : String) extends AjaxApiRequest("file", "rdcomp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile(component, self, CfsOpenOptions.Default) {
                    case comp : Component ⇒
                        comp.getData flatMap { cdesc ⇒
                            Full(Map("status" → 1, "component" → cdesc))
                        }
                    case other ⇒
                        other close()
                        Failure(s"$component is not a component")
                } flatMap MapResponse
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxWriteComponent(component : String, content : ComponentDescriptor)
        extends AjaxApiRequest("file", "wrcomp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (sclient.loggedIn_?) {
                Cfs.withExistingFile (component, self, CfsOpenOptions.Default) {
                    case comp : Component ⇒
                        val result = comp putData content flatMap (_ ⇒ SuccessResponse)
                        comp close ()
                        result
                    case other ⇒
                        other close ()
                        Failure(s"$component is not a component")
                }
            }
            else Failure("not logged in")
        }
    }

    sealed case class AjaxRunLua(path : String, args : Option[JArray], options : Option[LuaRunOptions],
                                 asOwner : Option[Boolean])
        extends AjaxApiRequest("file", "lua") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val runAsOwner = asOwner getOrElse false
            Cfs.withExistingFile (path, self, CfsOpenOptions.Default) {
                case luafile : CfsLuaScript ⇒
                    val runOptions = options getOrElse LuaRunOptions()
                    val outbox =
                        if (runAsOwner) luafile.runAsOwner(args, runOptions) else luafile.runAsSelf(args, runOptions)
                    outbox match {
                        case Full((status, result)) ⇒ MapResponse(Map("status" → status, "result" → result))
                        case e : EmptyBox ⇒ e
                    }
            }
        }
    }

    sealed case class AjaxRunJavaScript(path : String, args : Option[JArray], options : Option[JsRunOptions],
                                        asOwner : Option[Boolean])
        extends AjaxApiRequest("file", "jsrun") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val runAsOwner = asOwner getOrElse false
            Cfs.withExistingFile (path, self, CfsOpenOptions.Default) {
                case jsfile : CfsJsScript ⇒
                    val runOptions = options getOrElse JsRunOptions()
                    val outbox =
                        if (runAsOwner) jsfile.runAsOwner(args, runOptions) else jsfile.runAsSelf(args, runOptions)
                    outbox match {
                        case Full((status, result)) ⇒ MapResponse(Map("status" → status, "result" → result))
                        case e : EmptyBox ⇒ e
                    }
            }
        }
    }

    sealed case class AjaxRunQueuedScript(queue : String, filename : String, event : String, eventArgs : Array[JValue])
        extends AjaxApiRequest("file", "qsrun") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val cfsFileLib = new CfsFileLib(() ⇒ self)
            cfsFileLib.runQueuedScript(queue, filename, event, eventArgs) match {
                case Full((status, result)) ⇒
                    MapResponse(Map("status" → status, "result" → result))
                case e : EmptyBox ⇒ e
            }
        }
    }

    sealed case class AjaxMakeMailer(path : String, settings : MailerSettings)
        extends AjaxApiRequest("file", "mkmailer") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs.withValidPath(path) { cfspath ⇒
                Cfs.ifNoFile(cfspath, self) { cfspath ⇒
                    val options = new MailerCreateOptions(settings, 0L, millis, 0L, false)
                    Cfs.create(cfspath, self, CfsMailer.getMimeType, options) match {
                        case Full(mailer : CfsMailer) ⇒
                            val info = mailer.asMap
                            mailer close ()
                            MapResponse(info)
                        case Full(other) ⇒
                            other close ()
                            Failure(s"$path is somehow not a mailer")
                        case e : EmptyBox ⇒ e
                    }
                }
            }
        }
    }

    sealed case class AjaxSMTP(message : MailerMessage, mailer : Option[String]) extends AjaxApiRequest("file", "smtp") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            val mailerPath = mailer getOrElse Startup.SystemMailerPath
            Cfs.withExistingFile(mailerPath, self, CfsOpenOptions.Default) {
                case mailer : CfsMailer ⇒
                    mailer.send(message) match {
                        case Full(_) ⇒
                            mailer close ()
                            SuccessResponse
                        case e : EmptyBox ⇒ e
                    }
            }
        }
    }

    sealed case class AjaxValidateCache() extends AjaxApiRequest("file", "validate") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (self.isSystemAdmin_?) {
                val map = DbManager.validateCache
                MapResponse(map ++ Map("status" → 1))
            }
            else Failure("system administrator only")
        }
    }

    sealed case class AjaxSetParameter(name : String, value : JValue) extends AjaxApiRequest("file", "setparm") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            if (self.isSystemAdmin_?) {
                name.toLowerCase match {
                    case "vnodecachelimit" ⇒
                        value match {
                            case JInt(limit) ⇒
                                val cursize = DbManager setVNodeCacheLimit limit.intValue()
                                MapResponse(Map("status" → 1, "used" → cursize))
                            case _ ⇒ Failure("integer value expected")
                        }
                    case "fsnamecachelimit" ⇒
                        value match {
                            case JInt(limit) ⇒
                                val cursize = DbManager setFsNameCacheLimit limit.intValue()
                                MapResponse(Map("status" → 1, "used" → cursize))
                            case _ ⇒ Failure("integer value expected")
                        }
                    case _ ⇒ Failure(s"unknown parameter $name")
                }
            }
            else Failure("system administrator only")
        }
    }

    sealed case class AjaxCacheFilterOp(cmd : String, uri : Option[String], resid : Option[Long])
        extends AjaxApiRequest("file", "fecache") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            cmd match {
                case "clear" ⇒
                    uri foreach CacheFilter.remove
                    resid foreach (id ⇒ CacheFilter.remove(ResourceId(id)))
                    if (uri.isEmpty && resid.isEmpty) CacheFilter.clear()
                    SimpleResponse(1, "ok")
                case "dump" ⇒
                    val (names, ids) = CacheFilter.dump()
                    MapResponse(Map(
                        "status" → 1,
                        "names" → names.map(p ⇒ Array(p._1, p._2)),
                        "ids" → ids.map(p ⇒ Array(p._1, p._2))
                    ))
                case _ ⇒ Failure(s"unknown command: $cmd")
            }
        }
    }

    sealed case class AjaxGetFile(path : String,
                                  mimeType : Option[String],
                                  incdir : Option[Boolean],
                                  serve : Option[Boolean]) extends AjaxApiRequest("file", "get") {

        override def getResponse(req : Req, sclient : SessionClient, self : Principal) : Box[LiftResponse] = {
            Cfs open (path, self, CfsOpenOptions.Default) match {
                case Full(folder : CfsFolder) ⇒ doGetContainer(folder, self, incdir getOrElse false)
                case Full(plain : CfsPlain) ⇒ doGetFile(plain, mimeType, serve getOrElse false)
                case Full(other) ⇒
                    other close ()
                    Failure(s"$path file type does not support get operation")
                case Empty ⇒ SimpleResponse(0, s"no such file: $path")
                case f : Failure ⇒ f

            }
        }
    }

    val optable : Map[String, AjaxApiRequestExtractor[AjaxRequest]] = Map (
        "addrole" → AjaxApiRequestExtractor[AjaxAddRole]("file", "addrole"),
        "acopy" → AjaxApiRequestExtractor[AjaxCopyAttributeValues]("file", "acopy"),
        "adefn" → AjaxApiRequestExtractor[AjaxDefineAttribute]("file", "adefn"),
        "aget" → AjaxApiRequestExtractor[AjaxGetAttributeValues]("file", "aget"),
        "aset" → AjaxApiRequestExtractor[AjaxSetAttributeValues]("file", "aset"),
        "avoa" → AjaxApiRequestExtractor[AjaxGetValuesOfAttribute]("file", "avoa"),
        "chown" → AjaxApiRequestExtractor[AjaxChown]("file", "chown"),
        "cleanup" → AjaxApiRequestExtractor[AjaxCleanupFiles]("file", "cleanup"),
        "edrole" → AjaxApiRequestExtractor[AjaxEditRole]("file", "edrole"),
        "fcmp" → AjaxApiRequestExtractor[AjaxFileCompletion]("file", "fcmp"),
        "fecache" → AjaxApiRequestExtractor[AjaxCacheFilterOp]("file", "fecache"),
        "gather" → AjaxApiRequestExtractor[AjaxGatherJson]("file", "gather"),
        "get" → AjaxApiRequestExtractor[AjaxGetFile]("file", "get"),
        "jsrun" → AjaxApiRequestExtractor[AjaxRunJavaScript]("file", "jsrun"),
        "link" → AjaxApiRequestExtractor[AjaxLink]("file", "link"),
        "list" → AjaxApiRequestExtractor[AjaxListFolder]("file", "list"),
        "lookup" → AjaxApiRequestExtractor[AjaxLookupPath]("file", "lookup"),
        "lsaccess" → AjaxApiRequestExtractor[AjaxListAccess]("file", "lsaccess"),
        "lspolicy" → AjaxApiRequestExtractor[AjaxListPolicy]("file", "lspolicy"),
        "lsrole" → AjaxApiRequestExtractor[AjaxListRole]("file", "lsrole"),
        "lua" → AjaxApiRequestExtractor[AjaxRunLua]("file", "lua"),
        "mkcomp" → AjaxApiRequestExtractor[AjaxCreateComponent]("file", "mkcomp"),
        "mkcsv" → AjaxApiRequestExtractor[AjaxMakeCsv]("file", "mkcsv"),
        "mkdir" → AjaxApiRequestExtractor[AjaxMakeFolder]("file", "mkdir"),
        "mkjar" → AjaxApiRequestExtractor[AjaxMakeJar]("file", "mkjar"),
        "mklib" → AjaxApiRequestExtractor[AjaxMakeLib]("file", "mklib"),
        "mkmailer" → AjaxApiRequestExtractor[AjaxMakeMailer]("file", "mkmailer"),
        "mkpolicy" → AjaxApiRequestExtractor[AjaxCreatePolicy]("file", "mkpolicy"),
        "mkrole" → AjaxApiRequestExtractor[AjaxCreateRole]("file", "mkrole"),
        //                            "mkweb" → AjaxApiRequestExtractor[AjaxMakeWebSite]("file", "mkweb"),
        "mtlist" → AjaxApiRequestExtractor[AjaxListType]("file", "mtlist"),
        "pcomp" → AjaxApiRequestExtractor[AjaxParseComponent]("file", "pcomp"),
        "pplist" → AjaxApiRequestExtractor[AjaxPPList]("file", "pplist"),
        "pshare" → AjaxApiRequestExtractor[AjaxPShare]("file", "pshare"),
        "publish" → AjaxApiRequestExtractor[AjaxPublish]("file", "publish"),
        "punshare" → AjaxApiRequestExtractor[AjaxPUnshare]("file", "punshare"),
        "qsrun" → AjaxApiRequestExtractor[AjaxRunQueuedScript]("file", "qsrun"),
        "rdcomp" → AjaxApiRequestExtractor[AjaxReadComponent]("file", "rdcomp"),
        "rdcsv" → AjaxApiRequestExtractor[AjaxReadCsv]("file", "rdcsv"),
        "rmrole" → AjaxApiRequestExtractor[AjaxRmRole]("file", "rmrole"),
        "rm" → AjaxApiRequestExtractor[AjaxRemoveFile]("file", "rm"),
        "save" → AjaxApiRequestExtractor[AjaxSaveString]("file", "save"),
        "setmt" → AjaxApiRequestExtractor[AjaxSetMimeType]("file", "setmt"),
        "setparm" → AjaxApiRequestExtractor[AjaxSetParameter]("file", "setparm"),
        "share" → AjaxApiRequestExtractor[AjaxShare]("file", "share"),
        "smtp" → AjaxApiRequestExtractor[AjaxSMTP]("file", "smtp"),
        "unshare" → AjaxApiRequestExtractor[AjaxUnshare]("file", "unshare"),
        "wrcomp" → AjaxApiRequestExtractor[AjaxWriteComponent]("file", "wrcomp"),
        "validate" → AjaxApiRequestExtractor[AjaxValidateCache]("file", "validate")
    )

    val requestExtractor = new AjaxRequestTable(optable)

    def handleOp(req : Req) : Box[LiftResponse] = {

        requestExtractor.getReq(req) match {
            case Full(ajax) ⇒ ajax processRequest req
            case f : Failure ⇒ FailureResponse(f)
            case Empty ⇒
                req.param("op") match {
                    case Full("load") ⇒
                        AjaxLoadFile(req) processRequest req
                    case Full(op) ⇒ SimpleResponse(-1, s"unknown file request: $op")
                    case _ : EmptyBox ⇒ SimpleResponse(-1, "missing parameters for file operation")
                }
        }
    }

    def handleGet(req : Req) : Box[LiftResponse] = {
        val incdir = S.param("incdir").flatMap(tf ⇒ tryo(tf.toBoolean)) openOr false
        val serve = S.param("serve") map (tf ⇒ tf != "0" && tf != "false") openOr false
        val mimeType = S.param("type").toOption
        // Helper function to look for folder when Lift adds "index" to URL
        // ending in "/"
        def withFile(pp : ParsePath, by : Principal) : Box[LiftResponse] = {
            val fname = pp.wholePath.mkString("/", "/", "")
            Cfs open (fname, by, CfsOpenOptions.Default) match {
                case Full(folder : CfsFolder) ⇒ doGetContainer(folder, by, incdir)
                case Full(plain : CfsPlain) ⇒ doGetFile(plain, mimeType, serve)
                case Full(other) ⇒
                    other close ()
                    Failure(s"$fname file type does not support GET")
                case Empty ⇒
                    val partPath = pp.partPath
                    val last = if (partPath == Nil) "" else partPath.last
                    if (pp.suffix == "" && last == "index") {
                        // Strip the "index" that Lift added, and see if it's a folder
                        val dname = partPath.dropRight(1).mkString("/", "/", "")
                        Cfs.withValidPath (dname) { dpath ⇒
                            val folder = Cfs.openFolder(dpath, by) { vfile ⇒
                                vfile close ()
                                Failure(s"did not find $fname and $dname is not a folder}")
                            } ?~ s"found neither $fname nor $dname"
                            folder flatMap (doGetContainer(_, by, incdir))
                        }
                    }
                    else Failure(s"$fname not found")
                case f : Failure ⇒ f
            }
        }
        SessionClient.withCurrentUser { (uinfo, _) ⇒
            val self = uinfo.getSelfPrincipal
            withFile (req.path, self) match {
                case full @ Full(_) ⇒ full
                case Empty ⇒ Full(NotFoundResponse())
                case f : Failure ⇒ Full(InMemoryResponse(f.msg.getBytes,
                                         S.getResponseHeaders(Nil), S.responseCookies, 400))
            }
        }
    }

    def doGetContainer(folder : CfsFolder, by : Principal, incdir : Boolean) : Box[LiftResponse] = {
        val zipname = s"${folder.getName}.zip"
        val folderPath = folder.getPath
        folder close ()
        DbZip(folderPath, incdir, by) match {
            case Full(data) ⇒
                Log.info(s"zipped $zipname to ${data.length} bytes")
                Full(InMemoryResponse(data,
                    List(("Content-Length", data.length.toString),
                        ("Content-Type", "application/zip"),
                        ("Content-Disposition", "attachment; filename=\"" + zipname + "\"")) :::
                        S.getResponseHeaders(Nil), S.responseCookies, 200))
            case e : EmptyBox ⇒
                Log.error(s"zip of $zipname failed", e)
                e
        }
    }

    def doGetFile(file : CfsPlain, mimeType : Option[String], serve : Boolean) : Box[LiftResponse] = {
        file info () flatMap { info ⇒
            S.request flatMap { req ⇒
                val mtype = mimeType getOrElse info.mimeType
                S.setHeader("Content-Type", mtype)
                tryo(CfsFiles.newInputStream(file)) flatMap { in ⇒
                    if (serve && (mtype == "text/html" || mtype == "application/xhtml+xml")) {
                        Component.getDefaultComponent foreach { component ⇒
                            LiftRules.externalTemplateResolver.request.set(() ⇒
                                RoutePlainFile.templateResolver(component))
                        }
                        val tresult = S.htmlProperties.htmlParser(in) flatMap { nodeseq ⇒
                            val stripped = Templates.checkForContentId(nodeseq)
                            S.session flatMap { liftSession ⇒
                                liftSession.processTemplate(Full(stripped), req, req.path, 200)
                            }
                        }
                        in close()
                        tresult
                    }
                    else {
                        def endResponse() : Unit = {
                            in.close()
                        }
                        S.setHeader("Content-Length", info.size.toString)
                        if (!serve) {
                            S.setHeader("Content-Disposition", s"""attachment; filename="${file.getName}"""")
                        }
                        Full(StreamingResponse(in, () ⇒ endResponse(), info.size,
                                                  S.getResponseHeaders(Nil), S.responseCookies, 200))
                    }
                }
            }
        }
    }

    case class DirStreamWrapper(dirstream : DirectoryStream[Path]) extends java.lang.Iterable[Path] {
        protected val myiterator : util.Iterator[Path] = dirstream.iterator()
        protected var closed = false

        def iterator() : java.util.Iterator[Path] = new java.util.Iterator[Path] {

            override def hasNext : Boolean = myiterator.hasNext
            override def next : Path = {
                try {
                    val result =
                        if (closed) throw new NoSuchElementException
                        else myiterator.next()
                    if (!myiterator.hasNext) dirstream.close()
                    result
                }
                catch {
                    case e : Exception ⇒
                        close()
                        throw e
                }
            }
            override def remove() : Unit = {
                throw new UnsupportedOperationException
            }
            def close() : Unit = {
                if (!closed) {
                    dirstream.close()
                    closed = true
                }
            }
        }
    }

    def listDirectory(dir : Path, recurse : Boolean = false, prune : List[String] = List()) : Stream[Path] = {
        import scala.collection.JavaConverters.iterableAsScalaIterableConverter
        DirStreamWrapper(Files.newDirectoryStream(dir)).asScala.flatMap { fp ⇒
            val afp = if (fp.isAbsolute) fp else fp.toAbsolutePath
            if (prune.contains(afp.getFileName.toString)) Stream()
            else if (recurse && Files.isDirectory(afp)) DirStreamWrapper(Files.newDirectoryStream(afp)).asScala
            else Stream(afp)
        }.toStream
    }
    
    /**
     * Copy a host file or directory into the DB filesystem. Newer files in the DB
     * filesystem are not replaced.
     * 
     * @param hpath     the host file or directory to copy
     * @param dbdir     the DB container in which to copy the file
     * @param recursive true if a host directory should be copied recursively
     * @param prune     a list of filenames to prune during a recursive copy
     * @param by        the principal for this operation
     * @return  the DB path of the copied file or directory
     */
    def loadFile(hpath : Path, dbdir : CfsFile, recursive : Boolean, prune : List[String], by : Principal) : Box[CfsPath] = {
        val habspath = hpath //.toAbsolutePath
        def loadDirectory(hdir : Path, folder : CfsFolder) : Unit = {
            Log.debug(s"loadDirectory called with $hdir")
            listDirectory(hdir, recurse = false, prune).foreach { hp ⇒
                Log.debug(s"loadDirectory sees $hp")
                val fullpath = hp.toAbsolutePath
                val hname = fullpath.getFileName.toString
                val crtime = Files.getLastModifiedTime(fullpath).toMillis
                if (Files.isDirectory(fullpath)) {
                    if (recursive) {
                        val subfolderbox : Box[CfsFolder] = folder.getMember(hname) match {
                            case Full(f : CfsFolder) ⇒ Full(f)
                            case Full(dbfile) ⇒ Failure(s"not replacing ${dbfile.getPath.toString} with folder")
                            case _ ⇒ folder makeFolder (hname, CfsCreateOptions(ctime = crtime))
                        }
                        subfolderbox match {
                            case Full(subfolder) ⇒
                                loadDirectory(fullpath, subfolder)
                                subfolder.close()
                            case e : EmptyBox ⇒
                                Log.error(s"failed to create folder: $hname", e)
                                e
                        }
                    }
                }
                else loadPlainFile(fullpath, folder, by)
            }
        }
        def loadPlainFile(hplain : Path, folder : CfsFolder, by : Principal) : Box[CfsPath] = {
            val principal = folder.getPrincipal
            implicit val principalImpl : () ⇒ Principal = () ⇒ principal
            // Check if DB file already exists
            val fullpath = hplain.toAbsolutePath
            val hname = fullpath.getFileName.toString
            val crtime = Files.getLastModifiedTime(fullpath).toMillis
            val fpath = folder.getPath / hname
            val (doit, oldfile) = folder getMember hname match {
                case Full(cfsfile : CfsFile) ⇒
                    cfsfile info None match {
                        case Full(info) ⇒
                            val filetime = info.mtime
                            // Don't replace /index.html if it already exists, even if it's older
                            if ((filetime < crtime) && (fpath.toString != "/index.html")) {
                                Log.info(s"deleting old version of ${fpath.toString}")
                                (true, Some(cfsfile))
                            }
                            else {
                                if (filetime > crtime) {
                                    Log.info(s"DB has more recent version of ${fpath.toString}")
                                }
                                cfsfile close ()
                                (false, None)
                            }
                        case _ : EmptyBox ⇒
                            Log.error(s"unable to get info for ${fpath.toString}")
                            (false, None)
                    }
                case Full(other) ⇒
                    Log.error(s"not replacing non-Cfs file ${other.getPath.toString}")
                    other close ()
                    (false, None)
                case Empty ⇒ (true, None)
                case _ : Failure ⇒
                    Log.error(s"error opening ${fpath.toString}")
                    (false, None)
            }
            if (doit) tryo(new BufferedInputStream(Files.newInputStream(fullpath))) match {
                case Full(inputStream) ⇒
                    try {
                        val mimetype = MimeTypeDetector.getMimeType(inputStream, Some(hname))
                        oldfile match {
                            case Some(curfile) ⇒
                                replaceFileAlt(inputStream, mimetype, folder, curfile, Some(crtime)) flatMap { cfsfile ⇒
                                    cfsfile close()
                                    Full(fpath)
                                }
                            case None ⇒
                                try {
                                    CfsFiles.copy(inputStream, fpath, MIME_TYPE(mimetype))
                                    Log.info(s"imported $hplain to ${fpath.toString}")
                                    Full(fpath)
                                }
                                catch {
                                    case _ : Throwable ⇒ Failure(s"failed to copy data to ${fpath.toString}")
                                }
                        }
                    }
                    finally {
                        inputStream close ()
                    }
                case e : EmptyBox ⇒
                    Log.error(s"failed to read $hplain", e)
                    e
            }
            else Full(fpath)
        }
        if (!Files.exists(hpath)) Failure(s"file $habspath does not exist")
        else {
            dbdir match {
                case dbfolder : CfsFolder ⇒
                    if (Files.isDirectory(hpath)) {
                        loadDirectory(habspath, dbfolder)
                        Full(dbfolder.getPath)
                    }
                    else loadPlainFile(habspath, dbfolder, by)
                case _ ⇒ Failure(s"${dbdir.getPath} is not a folder")
            }
        }
    }
    
    def updateDefaultDirectory() : Unit = {
        Log.info("Beginning default directory update")
        //val fuser = GlobalConfig.getSystemUser.get
        //val fgroup = GlobalConfig.getDefaultGroup.get
        val prune = List("META-INF", "WEB-INF")
        val sysuser = SystemPrincipal
        val dbroot = Cfs open (CfsRootPath, sysuser, CfsOpenOptions.Default) match {
            case Full(root : CfsFile) ⇒ root
            case Full(_) ⇒ sys.error("failed to open root folder")
            case e : EmptyBox ⇒ sys.error(e.toString)
        }
        loadFile(getContextPath, dbroot, recursive = true, prune, sysuser)
        dbroot close ()
    }

//    def extractDbRoot() {
//        Log.info("beginning DB root extraction")
//        val sysuser = SystemPrincipal
//        val dbroot = Cfs open (DbRootPath, sysuser, None) match {
//            case Full(root : CfsFile) ⇒ root
//            case Full(_) ⇒ sys.error("failed to open root folder")
//            case e : EmptyBox ⇒ sys.error(e.toString)
//        }
//        extractFolder(dbroot)
//        dbroot close ()
//    }
}

