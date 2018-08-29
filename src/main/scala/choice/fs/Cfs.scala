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
package choice.fs

import choice.access._
import choice.actor.DbManager
import choice.fs.vfs._
import choice.model._
import choice.parser.CfsPathParser
import net.liftweb.common.{Full, _}
import net.liftweb.db.{DB, SuperConnection}
import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftweb.util.Helpers.{millis, randomString}

import scala.annotation.tailrec

/**
 * This file defines the Choice File System (Cfs). It is a specialization of the Vfs
 * interfaces and structures. It is based on storing file metadata in a SQL database,
 * and file data in the host filesystem.
 *
 * Currently there are three database tables that capture file metadata. The FsName
 * table stores (container, member, name) tuples which assign a name to a member of
 * a container. The container and member fields reference entries in a Resource table.
 * Resource entries contain most of the primary metadata for a file including the
 * size, owner, reference count, and timestamps. If the file is not empty, the
 * Resource entry also references an entry in the DataNode table. DataNode entries
 * contain a reference to the data in the host file system, and also some additional
 * metadata, such as a hash of the data contents, which is used for data deduplication.
 *
 * The plan is that all files can act as containers for metadata files. This
 * would be somewhat like Alternate Data Streams in NTFS, except that a metadata
 * file can itself be a container for other files. A simple versioning could be
 * supported by making a "prevVersion" metadata file for a file which is being
 * modified. That metadata file could contain the version of the file prior to
 * modification, and that version could have a "prevVersion" metadata file
 * containing an even older version.
 */

/**
 * This class represents a volume in the Cfs filesystem. It identifies the
 * filesystem as "CFS", and provides a version number to distinguish different
 * versions of the same filesystem. A volume has a volume id, which ideally
 * should be globally unique. It also has a volume name, which should be
 * some name that is meaningful to a user.
 *
 * Some filesystems support mounting volumes of other filesystems within their
 * namespaces.
 */
trait Cfs extends Vfs {
    import choice.fs.Cfs.Log

    type FilePF[T] = PartialFunction[CfsFile, Box[T]]

    /** Return a short (but hopefully unique) name that identifies this filesystem type */
    def getType : String = "CFS"

    /** Return a version string for this version of the filesystem */
    def getVersion : String = "0.9"

    /**
     * Return the id of the volume associated with this Vfs instance. Ideally
     * volume ids for a filesystem should be globally unique, but it is
     * sufficient that they are unique within the system.
     *
     * @return the volume id
     */
    def getVolumeId : String

    /**
     * Return the volume name, typically a string assigned by a user with some
     * associated meaning.
     *
     * @return the volume name
     */
    def getVolumeName : String

    /**
     * Get a Vnode representing the root of the filesystem. This is the starting
     * point for resolving file paths within the filesystem. The returned Vnode
     * is used to establish a mount point in a filesystem that supports mounts.
     *
     * The reference count for the returned root Vnode is incremented, which
     * will keep it from being deallocated until it is released. See Vnode
     * release().
     *
     * @return the filesystem root Vnode
     */
    override def getRoot : CfsFnode = DbManager getRoot "/"

    /**
     * Return true if this filesystem will permit the root of another specified
     * filesystem within it. If the mount is permitted, it can be performed on a
     * Vnode which is the mount point in this filesystem.
     *
     * @param vfs - Vfs for another filesystem
     * @return true if mount is supported
     */
    override def canMount_?(vfs : Vfs) : Boolean = true

    /**
     * Check whether a component of a filename path is valid for this filesystem.
     * A boxed boolean is returned, so that specific problems can be identified
     * by different Failures.
     *
     * A filesystem could support filename components with almost any syntax, though
     * the upper-level path parsing may not permit "/" within a component.
     *
     * @param name - a component of a filename path
     * @return true if the component is valid, otherwise false
     */
    def isValidName_?(name : String) : Boolean = {
        (CfsPathParser filename name).isDefined
    }

    /**
     * Open the file referenced by a specified filename path on behalf of a given
     * principal.
     *
     * @param path the filename path
     * @param principal the principal to be associated with the returned VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if the specified file exists and is successfully opened.
     *         Empty if the file or any components of the path do not exist. A Failure
     *         will be returned for insufficient access rights or other errors that
     *         may occur.
     */
    override def open(path : VPath, principal : Principal, options : CfsOpenOptions) : Box[VFile] = {
        path match {
            case cfspath : CfsPath ⇒ cfsOpen(cfspath, principal, options)
            case _ ⇒ Failure(s"attempt to open non-Cfs path '${path.toString}' with Cfs")
        }
    }

    def cfsOpen(path : CfsPath, principal : Principal,
             options : CfsOpenOptions = CfsOpenOptions.Default) : Box[VFile] = {
        CfsFolder openRootFolder principal flatMap { root ⇒
            def helper(list : List[String], file : VFile) : Box[VFile] = {
                list match {
                    case Nil ⇒ Full(file)
                    case head :: tail ⇒
                        val result =
                            file match {
                                case dir : VDirFile ⇒
                                    dir open (head, principal, options) match {
                                        case Full(nfile) ⇒ helper(tail, nfile)
                                        case Empty ⇒
                                            if (tail == Nil) Empty
                                            else Failure(s"folder $head does not exist")
                                        case f : Failure ⇒ f
                                    }
                                case nondir : VFile ⇒ Failure(s"${nondir.getPath.toString} is not a folder")
                            }
                        file close ()
                        result
                }
            }
            helper (path.allParts, root)
        }
    }

    def open(path : String, principal : Principal, options : CfsOpenOptions = CfsOpenOptions.Default) : Box[VFile] = {
        withValidPath(path) { cfspath ⇒ cfsOpen(cfspath, principal, options) }
    }

    /**
     * Open a file by its file id.
     *
     * @param fileId the file id
     * @param principal the principal to be associated with the returned VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if the specified file exists and is successfully opened.
     *         Empty if the file or any components of the path do not exist. A Failure
     *         will be returned for insufficient access rights or other errors that
     *         may occur.
     */
    def open(fileId : CfsVFileId, principal : Principal, options : CfsOpenOptions) : Box[VFile] = {
        DbManager lookupById fileId match {
            case Full(vnode) ⇒
                DbManager findPath (vnode, principal) match {
                    case Full(path) ⇒ vnode open (path, principal, options)
                    case _ : EmptyBox ⇒
                        Failure(s"no path to file id ${fileId.resource} for ${principal.getPrincipalName}")
                }
            case e : EmptyBox ⇒ e
        }
    }

    private def notFolderDefault(vfile : VFile) : Box[CfsFolder] = {
        val path = vfile.getPath
        vfile close ()
        Failure(s"$path is not a folder")
    }

    def openFolderPath(path : String, principal : Principal,
                       options : CfsOpenOptions = CfsOpenOptions.Default)
                      (implicit notfolder : VFile ⇒ Box[CfsFolder] = notFolderDefault) : Box[CfsFolder] = {
        withValidPath (path) (openFolder(_, principal, options)(notfolder))
    }

    /**
     * Open a Cfs folder. If the specified path references something other than a CfsFolder,
     * a failure is returned. If the last component of the path names a non-existent file,
     * Empty is returned.
     *
     * @param path the path to the folder
     * @param principal the principal to be associated with the returned CfsFolder
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting CfsFolder
     * @param notfolder a function that is called when the path refers to a non-folder,
     *                  allowing for a custom error message
     * @return a boxed CfsFolder, Empty, or Failure
     */
    def openFolder(path : CfsPath, principal : Principal,
                   options : CfsOpenOptions = CfsOpenOptions.Default)
                  (implicit notfolder : VFile ⇒ Box[CfsFolder] = notFolderDefault) : Box[CfsFolder] = {
        cfsOpen(path, principal, options) match {
            case Full(folder : CfsFolder) ⇒ Full(folder)
            case Full(other) ⇒ notfolder(other)
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Create a new file at location specified by the filename path.
     *
     * Some things that can caused this to fail:
     *
     *     If the path preceding the last component of the path does not already
     *     exist. Some filesystems may support an option to create any path
     *     components that do not exist.
     *
     *     The MIME type also may prevent the file from being created, if the
     *     filesystem or the last container doesn't support the specified type.
     *
     *     The principal has insufficient access rights, either to traverse the
     *     path to where the file will be created, or to create the file in the
     *     final container.
     *
     * @param path the filename path
     * @param principal the principal responsible for the operation
     * @param mimeType the MIME type associated with the new file
     * @param options optional options that may affect the create
     * @return a boxed VFile if the file is created successfully, or a
     *         Failure otherwise. Empty should not be returned.
     */
    def create(path : VPath, principal : Principal, mimeType : String,
               options : CfsCreateOptions = CfsCreateOptions.Default) : Box[VFile] = {
        path.allParts match {
            case Nil ⇒ Failure("create: empty path")
            case list ⇒ open (path.getParent, principal, CfsOpenOptions.Default) match {
                case Full(dir : CfsDirFile) ⇒
                    val result = dir create (list.last, mimeType, options)
                    dir close ()
                    result
                case Full(other) ⇒
                    val ppath = other.getPath
                    other close ()
                    Failure(s"$ppath is not a container")
                case e : EmptyBox ⇒ e
            }
        }
    }

    /**
     * Determine whether a file exists at a given filename path.
     *
     * @param path the filename path
     * @param principal the principal responsible for this operation
     * @return Full(true) if the file exists if the file exists and is visible to
     *         the principal. Full(false) if the path up to the final component
     *         exists and is accessible, but the last component does not exist in
     *         the final container. Failure if an error occurs, including
     *         insufficient access to some part of the path. Empty should not be
     *         returned.
     */
    def exists(path : VPath, principal : Principal) : Box[Boolean] = {
        lookupVnode(getRoot, path.allParts) map (_ ⇒ true)
    }

    def withValidPath[T](path : String)(f : CfsPath ⇒ Box[T]) : Box[T] = {
        CfsPath(path) match {
            case Full(cfspath) ⇒ f(cfspath)
            case e : EmptyBox ⇒ e
        }
    }

    def withValidFilename[T](filename : String)(f : String ⇒ Box[T]) : Box[T] = {
        CfsPathParser filename filename match {
            case Full(_) ⇒ f(filename)
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * Remove a given file, optionally recursively. The specified file handle is
     * always closed, whether the operation succeeds or not.
     *
     * @param file file handle of file to be removed
     * @param recursive true if the operation should be recursive
     * @return a boxed Boolean which is true if the file was deleted, false if the
     *         file was removed but other links to it remain. Failure is returned
     *         if the principal lacked the proper access rights, or on any other error.
     */
    def remove(file : CfsFile, recursive : Boolean) : Box[Boolean] = {
        val ppath = file.getPath.getParent
        val bbox = Cfs open (ppath, file.getPrincipal, CfsOpenOptions.Default) match {
            case Full(dirfile : CfsDirFile) ⇒
                val result = dirfile unlink (file.getName, recursive)
                dirfile close ()
                result
            case Full(other) ⇒
                other close ()
                Failure(s"${ppath.toString} is not a folder")
            case e : EmptyBox ⇒ e
        }
        file close ()
        bbox
    }

    def createUniqueSubfolder(parent : CfsFolder, ctime : Long = millis) : CfsFolder = {
        def withRandomName(attempts : Int) : CfsFolder = {
            if (attempts >= 4) sys.error(s"createUniqueSubfolder failed for ${parent.getPath.toString}")
            val name = s"tmp${randomString(8)}"
            parent getMember name match {
                case Full(vfile) ⇒
                    vfile close ()
                    withRandomName(attempts + 1)
                case Empty ⇒
                    parent makeFolder name match {
                        case Full(child) ⇒ child
                        case _ : EmptyBox ⇒ withRandomName(attempts + 1)
                    }
                case _ : Failure ⇒ withRandomName(attempts + 1)
            }
        }
        val subfolder = withRandomName(0)
        val newName = subfolder.getResourceId.id.toString
        parent linkUnchecked (newName, subfolder) match {
            case Full(result : CfsFolder) ⇒
                parent unlinkUnchecked (subfolder, false)
                subfolder close ()
                result
            case Full(other) ⇒
                subfolder close ()
                other close ()
                sys.error("unexpected file type returned")
            case _ : EmptyBox ⇒
                subfolder close ()
                sys.error("error changing subfolder name")
        }
    }

    /**
     * Open an existing file and pass it to a partial function that returns a boxed
     * value. If the partial function is not defined for the file, return Failure.
     * Otherwise return the value it returned. If the specified file does not exist,
     * that is also a Failure.
     *
     * @param path the filename path of the file
     * @param principal the principal responsible for this operation
     * @param options options for opening the file
     * @param f the partial function
     * @tparam T the type returned boxed by the partial function
     * @return the partial function's boxed T if possible, otherwise Failure
     */
    def withExistingFile[T](path : CfsPath, principal : Principal,
                            options : CfsOpenOptions = CfsOpenOptions.Default)(f : FilePF[T]) : Box[T] = {
        open (path, principal, options) match {
            case Full(cfsfile : CfsFile) ⇒
                try {
                    (f orElse {
                        case _ ⇒ Failure(s"${path.toString} is not valid for this operation")
                    } : FilePF[T]) (cfsfile)
                }
                finally {
                    cfsfile close ()
                }
            case Full(vfile) ⇒
                vfile close ()
                Failure("operation not implemented for non-Cfs file")
            case Empty ⇒ Failure(s"${path.toString} does not exist")
            case f : Failure ⇒ f
        }
    }

    def withExistingFile[T](path : String, principal : Principal,
                            options : CfsOpenOptions)(f : FilePF[T]) : Box[T] = {
        withValidPath(path) { cfspath ⇒
            withExistingFile(cfspath, principal, options)(f)
        }
    }

    def withVnode[T](resourceId : ResourceId)(f : ⇒ CfsVnode ⇒ Box[T]) : Box[T] = {
        DbManager lookupById resourceId match {
            case Full(vnode) ⇒
                val result = f(vnode)
                vnode.release
                result
            case e : EmptyBox ⇒ e
        }
    }

    def ifNoFile[T](path : CfsPath, principal : Principal,
                    options : CfsOpenOptions = CfsOpenOptions.Default)(f : CfsPath ⇒ Box[T]) : Box[T] = {
        open (path, principal, options) match {
            case Full(vfile) ⇒
                vfile close ()
                Failure(s"'${path.toString} already exists")
            case Empty ⇒ f (path)
            case fail : Failure ⇒ fail
        }
    }

    def withPolicyNode[T](policyId : ResourceId)(f : PolicyNode ⇒ Box[T]) : Box[T] = {
        DbManager lookupById policyId match {
            case Full(pnode : PolicyNode) ⇒
                val result = f(pnode)
                pnode.release
                result
            case Full(vnode) ⇒
                val result = Failure(s"withPolicyNode: resource ${policyId.id} is not a policy")
                Log.error(result)
                vnode.release
                result
            case Empty ⇒
                val result = Failure(s"withPolicyNode: policy id ${policyId.id} does not exist")
                Log.error(result)
                result
            case f : Failure ⇒
                Log.error(s"withPolicyNode: failure", f)
                f
        }
    }

    def withRoleNode[T](roleId : RoleId)(f : RoleNode ⇒ Box[T]) : Box[T] = {
        DbManager lookupById ResourceId(roleId.id) match {
            case Full(rnode : RoleNode) ⇒
                val result = f(rnode)
                rnode.release
                result
            case Full(vnode) ⇒
                val result = Failure(s"withRoleNode: resource ${roleId.id} is not a role")
                Log.error(result)
                vnode.release
                result
            case Empty ⇒
                val result = Failure(s"withRoleNode: role id ${roleId.id} does not exist")
                Log.error(result)
                result
            case f : Failure ⇒
                Log.error(s"withRoleNode: failure", f)
                f
        }
    }

    def expandFileSpec(fspec : String, principal : Principal) : Stream[String] = {
        def recurseHelper(cpath : String) : Stream[String] = {
            val members = Cfs.withExistingFile(cpath, principal, CfsOpenOptions.Default /*, Some(Map("list" -> true)) */) {
                case container : CfsDirFile ⇒
                    container getMembers () map (_ map (cpath + "/" + _))
            }
            (members openOr Nil).toStream
        }
        def recurseAllHelper(paths : List[String]) : Stream[String] = {
            paths match {
                case Nil ⇒
                    Stream.Empty
                case head :: tail ⇒
                    val members = Cfs.withExistingFile(head, principal, CfsOpenOptions.Default /*, Some(Map("list" -> true)) */) {
                        case container : CfsDirFile ⇒
                            container getMembers () map { list ⇒
                                list map (head + "/" + _)
                            }
                    } openOr Nil
                    head #:: recurseAllHelper(tail ::: members.toList)
            }
        }
        val (recurse, recurseAll, cpath) =
            if (fspec endsWith "/**") (false, true, if (fspec.length == 3) "/" else fspec.dropRight(3))
            else if (fspec endsWith "/*") (true, false, if (fspec.length == 2) "/" else fspec.dropRight(2))
            else (false, false, fspec)
        if (!(recurse || recurseAll)) Stream(cpath)
        else if (recurse) cpath #:: recurseHelper(cpath)
        else recurseAllHelper(List(cpath))
    }

    @tailrec
    protected final def lookupVnode(container : VDirNode, path : Seq[String]) : Box[Vnode] = {
        path match {
            case Nil ⇒ Full(container)
            case head :: tail ⇒
                if (isValidName_? (head)) {
                        container lookup head match {
                            case Full(vnode : VDirNode) ⇒ lookupVnode(vnode, tail)
                            case Full(vnode) ⇒
                                if (tail == Nil) Full(vnode)
                                else Failure(s"'$head' is not a container")
                            case e : EmptyBox ⇒ e
                        }
                }
                else Failure(s"'$head' is not a valid path component")
        }
    }
}

/**
 * This object initializes the default Cfs filesystem.
 */
object Cfs extends Cfs with Module {
    override protected[fs] val Log = Logger("choice.fs.Cfs")

    override val getName = "Cfs Filesystem"

    override val getSchemas : List[BaseMetaMapper] = List(DataNode, Resource, FsName)

    override val getRights : List[RightDef] = Nil

    lazy val defaultDbUrl : String = (DB use DefaultConnectionIdentifier) { conn : SuperConnection ⇒
        conn.connection.getMetaData.getURL
    }

    def getVolumeId : String = defaultDbUrl

    def getVolumeName : String = "Cfs Default"
}

case class CfsInfo(override val mimeType : String,
                   override val ctime : Long,
                   override val mtime : Long,
                   override val size : Long,
                   override val refCount : Long,
                   altid : Option[Long]
                  ) extends VInfo(mimeType, ctime, mtime, size, refCount)
