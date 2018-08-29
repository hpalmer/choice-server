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
  * @author Howard Palmer
  */
package choice.fs.vfs

import choice.fs.{CfsCreateOptions, CfsOpenOptions}
import net.liftweb.common._
import choice.access.Principal

///**
// * Represent a filename path. Initially this just wraps a string, but it may be
// * expanded to include path operations.
// *
// * @param path the path string
// */
//case class XPath(path : String) extends VPath(path, DbRootPath, DbRootPath) {
//    def split : Array[String] = {
//        path split "/" filter (_ != "")
//    }
//    def ofMember(name : String) : XPath = {
//        XPath((split + name).mkString("/", "/", ""))
//    }
//    def ofParent : XPath = XPath((split dropRight 1).mkString("/", "/", ""))
//}

/**
 * This trait defines an interface to a filesystem, much as with Unix kernels.
 * Each different type of filesystem will provide this interface. A separate
 * instance will be created for each different volume.
 */
trait Vfs {

    /** Return a short (but hopefully unique) name that identifies this filesystem type */
    def getType : String

    /** Return a version string for this version of the filesystem */
    def getVersion : String

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
     * @return
     */
    def getRoot : Vnode

    /**
     * Return true if this filesystem will permit the root of another specified
     * filesystem within it. If the mount is permitted, it can be performed on a
     * Vnode which is the mount point in this filesystem.
     *
     * @param vfs - Vfs for another filesystem
     * @return true if mount is supported
     */
    def canMount_?(vfs : Vfs) : Boolean

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
    def isValidName_?(name : String) : Boolean

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
    def open(path : VPath, principal : Principal, options : CfsOpenOptions) : Box[VFile]

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
     * @param options options that may affect the create
     * @return a boxed VFile if the file is created successfully, or a
     *         Failure otherwise. Empty should not be returned.
     */
    def create(path : VPath, principal : Principal, mimeType : String,
               options : CfsCreateOptions = CfsCreateOptions.Default) : Box[VFile]

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
    def exists(path : VPath, principal : Principal) : Box[Boolean]
}

object Vfs {
    import net.liftweb.util.Helpers.tryo
    val Log = Logger("choice.fs.Vfs")

    /**
     * Extract a named Long value from an options map. The value can exist as an
     * integer or a string in the map.
     *
     * @param name the option name
     * @param options the option map
     * @param default the default value
     * @return the extracted value if successful, otherwise default
     */
    def getLongOption(name : String, options : Map[String, Any], default : Long) : Long = {
        options get name map {
            case x : Long ⇒ x
            case x : Int ⇒ x.toLong
            case x : String ⇒ tryo(x.toLong) openOr default
            case x ⇒
                Log.error(s"invalid option value: $x")
                default
        } getOrElse default
    }

    def getLongOption(name : String, options : Option[Map[String, Any]], default : Long) : Long = {
        options map (getLongOption (name, _, default)) getOrElse default
    }

    def getBooleanOption(name : String, options : Map[String, Any], default : Boolean) : Boolean = {
        options get name map {
            case x : Boolean ⇒ x
            case "true" | "yes" | "on" | "enable" ⇒ true
            case "false" | "no" | "off" | "disable" ⇒ false
            case x ⇒
                Log.error(s"invalid option value: $x")
                default
        } getOrElse default
    }

    def getBooleanOption(name : String, options : Option[Map[String, Any]], default : Boolean) : Boolean = {
        options map (getBooleanOption (name, _, default)) getOrElse default
    }
}




/**
 * This defines an abstract identifier for a file within a particular filesystem.
 * All the files within a given instance (e.g. volume) of a filesystem should have
 * a unique VFileId. The internal structure of a VFileId is opaque to code
 * outside the filesystem in which it is used.
 */
trait VFileId {

    /**
     * Check whether a given file id is the same as this one. The id should
     * always be from the same filesystem as this one.
     *
     * @param id - some file id from the same filesystem
     * @return - true if the file ids match
     */
    def isFileId_?(id : VFileId) : Boolean
}


/**
 * Information returned on a VFile.info() operation. Some filesystems may subclass
 * this class and return additional information.
 *
 * @param mimeType the file MIME type (which may be an educated guess)
 * @param ctime the creation timestamp of the file
 * @param mtime the modification timestamp of the file
 * @param size the size of the file
 * @param refCount count of references to this file
 */
class VInfo (
    val mimeType : String,
    val ctime : Long,
    val mtime : Long,
    val size : Long,
    val refCount : Long
)
