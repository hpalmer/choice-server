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
  * Representations of a file path in the Choice File System.
  *
  * @author Howard Palmer
  */
package choice.fs

import choice.parser.CfsPathParser
import net.liftweb.common.{Failure, EmptyBox, Box, Full}
import java.nio.file.InvalidPathException
import choice.fs.vfs.{VPath, Vfs}

/**
 * This implements the VPath trait for the Cfs filesystem.
 */

abstract class CfsPath extends VPath {
    /**
     * Returns the file system that created this object.
     *
     * @return the file system that created this object
     */
    def getFileSystem : Vfs = Cfs

    def getParent : CfsPath

    def resolve(cfspath : CfsPath) : CfsPath

    def allParts : List[String]

    def /(name : String) : CfsPath

    /**
     * Helper function to compare paths.
     *
     * @param parts the name components of one path
     * @param otherparts the name components of another path
     * @return zero if the paths are identical
     *         negative if the first path precedes the second lexicographically
     *         positive if the second precedes the first
     */
    def compareParts(parts : List[String], otherparts : List[String]) : Int = {
        (parts, otherparts) match {
            case (Nil, Nil) ⇒ 0
            case (Nil, notnil) ⇒ 0 - notnil.head.charAt(0).toInt
            case (notnil, Nil) ⇒ notnil.head.charAt(0).toInt
            case (head :: tail, ohead :: otail) ⇒
                val cmp = head compareTo ohead
                if (cmp == 0) compareParts(tail, otail)
                else cmp
        }
    }
}

case class CfsRootPath(parts : List[String]) extends CfsPath {
    /**
     * Tells whether or not this path is absolute.
     *
     * An absolute path is complete in that it doesn't need to be combined with other path
     * information in order to locate a file.
     *
     * @return true if, and only if, this path is absolute
     */
    def isAbsolute : Boolean = true

    /**
     * Returns the root component of this path as a VPath object, or null if this path does
     * not have a root component.
     *
     * @return a path representing the root component of this path, or null
     */
    def getRoot : CfsRootPath = this

    /**
     * Returns the name of the file or directory denoted by this path as a VPath object.
     * The file name is the farthest element from the root in the directory hierarchy.
     *
     * @return a path representing the name of the file or directory, or null if this path
     *         has zero elements
     */
    def getFileName : CfsPath = null

    /**
     * Returns the parent path, or null if this path does not have a parent.
     *
     * The parent of this path object consists of this path's root component, if any, and
     * each element in the path except for the farthest from the root in the directory hierarchy.
     * This method does not access the file system; the path or its parent may not exist.
     * Furthermore, this method does not eliminate special names such as "." and ".." that
     * may be used in some implementations. On UNIX for example, the parent of "/a/b/c" is "/a/b",
     * and the parent of "x/y/." is "x/y". This method may be used with the normalize method, to
     * eliminate redundant names, for cases where shell-like navigation is required.
     *
     * If this path has one or more elements, and no root component, then this method is
     * equivalent to evaluating the expression:
     *
     * subpath(0, getNameCount()-1)
     *
     * @return a path representing the path's parent
     */
    def getParent : CfsRootPath = this

    /**
     * Returns the number of name elements in the path.
     *
     * @return the number of elements in the path, or 0 if this path only represents
     *         a root component
     */
    def getNameCount : Int = 0

    /**
     * Returns a name element of this path as a Path object.
     *
     * The index parameter is the index of the name element to return. The element that is
     * closest to the root in the directory hierarchy has index 0. The element that is farthest
     * from the root has index count-1.
     *
     * @param index the index of the element
     * @return the name element
     * @throws IllegalArgumentException - if index is negative, index is greater than or equal
     *         to the number of elements, or this path has zero name elements
     */
    def getName(index : Int) : CfsPath = throw new IllegalArgumentException("getName on CfsRootPath")

    /**
     * Returns a relative VPath that is a subsequence of the name elements of this path.
     *
     * The beginIndex and endIndex parameters specify the subsequence of name elements. The name
     * that is closest to the root in the directory hierarchy has index 0. The name that is
     * farthest from the root has index count-1. The returned VPath object has the name elements
     * that begin at beginIndex and extend to the element at index endIndex-1.
     *
     * @param beginIndex the index of the first element, inclusive
     * @param endIndex the index of the last element, exclusive
     * @return a new VPath object that is a subsequence of the name elements in this VPath
     * @throws IllegalArgumentException -  if beginIndex is negative, or greater than or equal
     *         to the number of elements. If endIndex is less than
     *         or equal to beginIndex, or larger than the number
     *         of elements.
     */
    def subpath(beginIndex : Int, endIndex : Int) : CfsRelativePath =
        throw new IllegalArgumentException("subpath on CfsRootPath")

    /**
     * Tests if this path starts with the given path.
     *
     * This path starts with the given path if this path's root component starts with the root
     * component of the given path, and this path starts with the same name elements as the
     * given path. If the given path has more name elements than this path then false is returned.
     *
     * Whether or not the root component of this path starts with the root component of the given
     * path is file system specific. If this path does not have a root component and the given path
     * has a root component then this path does not start with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false is returned.
     *
     * @param other the given path
     * @return true if this path starts with the given path; otherwise false
     */
    def startsWith(other : VPath) : Boolean = other match {
        case rpath : CfsRootPath ⇒ parts == rpath.parts
        case apath : CfsAbsolutePath ⇒ apath.parts == Nil && startsWith(apath.root)
        case _ ⇒ false
    }

    /**
     * Tests if this path starts with a Path, constructed by converting the given path string,
     * in exactly the manner specified by the startsWith(Path) method. On UNIX for example,
     * the path "foo/bar" starts with "foo" and "foo/bar". It does not start with "f" or "fo".
     *
     * @param other the given path string
     * @return true if this path starts with the given path; otherwise false
     * @throws InvalidPathException - If the path string cannot be converted to a VPath
     */
    def startsWith(other : String) : Boolean = {
        CfsPathParser path (other, this) match {
            case Full(path) ⇒ startsWith(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Tests if this path ends with the given path.
     *
     * If the given path has N elements, and no root component, and this path has N or more
     * elements, then this path ends with the given path if the last N elements of each path,
     * starting at the element farthest from the root, are equal.
     *
     * If the given path has a root component then this path ends with the given path if the
     * root component of this path ends with the root component of the given path, and the
     * corresponding elements of both paths are equal. Whether or not the root component of
     * this path ends with the root component of the given path is file system specific. If
     * this path does not have a root component and the given path has a root component then
     * this path does not end with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false
     * is returned.
     *
     * @param other the given path
     * @return true if this path ends with the given path; otherwise false
     */
    def endsWith(other : VPath) : Boolean = other match {
        case rootpath : CfsRootPath ⇒ parts endsWith rootpath.parts
        case abspath : CfsAbsolutePath ⇒
            (parts endsWith abspath.root.parts) && (abspath.parts == Nil)
        case relpath : CfsRelativePath ⇒ parts endsWith relpath.parts
        case _ ⇒ false
    }

    /**
     * Tests if this path ends with a Path, constructed by converting the given path string,
     * in exactly the manner specified by the endsWith(VPath) method. On UNIX for example,
     * the path "foo/bar" ends with "foo/bar" and "bar". It does not end with "r" or "/bar".
     * Note that trailing separators are not taken into account, and so invoking this method
     * on the VPath "foo/bar" with the String "bar/" returns true.
     *
     * @param other the given path string
     * @return true if this path starts with the given path; otherwise false
     * @throws InvalidPathException - If the path string cannot be converted to a VPath
     */
    def endsWith(other : String) : Boolean = {
        CfsPathParser path (other, this) match {
            case Full(path) ⇒ endsWith(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Resolve the given path against this path.
     *
     * If the other parameter is an absolute path then this method trivially returns other.
     * If other is an empty path then this method trivially returns this path. Otherwise this
     * method considers this path to be a directory and resolves the given path against this
     * path. In the simplest case, the given path does not have a root component, in which
     * case this method joins the given path to this path and returns a resulting path that
     * ends with the given path. Where the given path has a root component then resolution
     * is highly implementation dependent and therefore unspecified.
     *
     * @param other the path to resolve against this path
     * @return the resulting path
     */
    def resolve(other : VPath) : VPath = {
        if (other.isAbsolute) other
        else other match {
            case relpath : CfsRelativePath ⇒ CfsAbsolutePath(this, relpath.parts)
            case _ ⇒ other
        }
    }

    /**
     * Converts a given path string to a Path and resolves it against this Path in exactly
     * the manner specified by the resolve method. For example, suppose that the name
     * separator is "/" and a path represents "foo/bar", then invoking this method with
     * the path string "gus" will result in the Path "foo/bar/gus".
     *
     * @param other the path string to resolve against this path
     * @return the resulting path
     * @throws InvalidPathException - if the path string cannot be converted to a VPath
     */
    def resolve(other : String) : VPath = {
        CfsPathParser path (other, this) match {
            case Full(path) ⇒ resolve(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Resolves the given path against this path's parent path. This is useful where a
     * file name needs to be replaced with another file name. For example, suppose that
     * the name separator is "/" and a path represents "dir1/dir2/foo", then invoking this
     * method with the VPath "bar" will result in the VPath "dir1/dir2/bar". If this path
     * does not have a parent path, or other is absolute, then this method returns other.
     * If other is an empty path then this method returns this path's parent, or where
     * this path doesn't have a parent, the empty path.
     *
     * @param other the path to resolve against this path's parent
     * @return the resulting path
     */
    def resolveSibling(other : VPath) : VPath = getParent resolve other

    /**
     * Converts a given path string to a VPath and resolves it against this path's parent path
     * in exactly the manner specified by the resolveSibling method.
     *
     * @param other the path string to resolve against this path's parent
     * @return the resulting path
     * @throws InvalidPathException - if the path string cannot be converted to a VPath
     */
    def resolveSibling(other : String) : VPath = {
        CfsPathParser path other match {
            case Full(path) ⇒ resolveSibling(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Compares two abstract paths lexicographically. The ordering defined by this method
     * is provider specific, and in the case of the default provider, platform specific.
     * This method does not access the file system and neither file is required to exist.
     *
     * This method may not be used to compare paths that are associated with different
     * file system providers.
     *
     * @param other the path compared to this path.
     * @return zero if the argument is equal to this path, a value less than zero if this
     *         path is lexicographically less than the argument, or a value greater than
     *         zero if this path is lexicographically greater than the argument
     * @throws ClassCastException - if the paths are associated with different providers
     */
    def compareTo(other : VPath) : Int = other match {
        case rootpath : CfsRootPath ⇒ compareParts(parts, rootpath.parts)
        case abspath : CfsAbsolutePath ⇒ compareParts(parts, abspath.root.parts ++ abspath.parts)
        case relpath : CfsRelativePath ⇒ compareParts(parts, relpath.parts)
        case _ ⇒ throw new ClassCastException(s"CfsRootPath compareTo ${other.getClass.getName}")
    }

    def resolve(cfspath : CfsPath) : CfsPath = cfspath match {
        case relpath : CfsRelativePath ⇒ CfsAbsolutePath(this, relpath.parts)
        case other ⇒ other
    }

    def allParts : List[String] = parts

    def /(name : String) : CfsAbsolutePath = {
        CfsPathParser filename name match {
            case Full(relpath) ⇒ CfsAbsolutePath(this, relpath.parts)
            case _ ⇒ throw new InvalidPathException(name, "invalid Cfs name component")
        }
    }

    override def toString : String = allParts.mkString("/", "/", "")
}

/**
  * The Cfs filesystem root as an absolute path.
  */
object CfsRootPath extends CfsAbsolutePath(CfsRootRoot, Nil)

trait CfsPathParts { self : CfsPath ⇒

    def parts : List[String]

    /**
     * Returns the name of the file or directory denoted by this path as a VPath object.
     * The file name is the farthest element from the root in the directory hierarchy.
     *
     * @return a path representing the name of the file or directory, or null if this path
     *         has zero elements
     */
    def getFileName : VPath = parts match {
        case Nil ⇒ null
        case list ⇒ CfsRelativePath(List(list.last))
    }

    /**
     * Returns the number of name elements in the path.
     *
     * @return the number of elements in the path, or 0 if this path only represents
     *         a root component
     */
    def getNameCount : Int = parts.length

    /**
     * Returns a name element of this path as a VPath object.
     *
     * The index parameter is the index of the name element to return. The element that is
     * closest to the root in the directory hierarchy has index 0. The element that is farthest
     * from the root has index count-1.
     *
     * @param index the index of the element
     * @return the name element
     * @throws IllegalArgumentException - if index is negative, index is greater than or equal
     *         to the number of elements, or this path has zero name elements
     */
    def getName(index : Int) : CfsRelativePath = index match {
        case n if n < 0 || n >= parts.length ⇒
            throw new IllegalArgumentException(s"getName on CfsRelativePath bad index: $n")
        case n ⇒ CfsRelativePath(List(parts(n)))
    }

    /**
     * Returns a relative VPath that is a subsequence of the name elements of this path.
     *
     * The beginIndex and endIndex parameters specify the subsequence of name elements. The name
     * that is closest to the root in the directory hierarchy has index 0. The name that is
     * farthest from the root has index count-1. The returned VPath object has the name elements
     * that begin at beginIndex and extend to the element at index endIndex-1.
     *
     * @param beginIndex the index of the first element, inclusive
     * @param endIndex the index of the last element, exclusive
     * @return a new VPath object that is a subsequence of the name elements in this VPath
     * @throws IllegalArgumentException -  if beginIndex is negative, or greater than or equal
     *         to the number of elements. If endIndex is less than
     *         or equal to beginIndex, or larger than the number
     *         of elements.
     */
    def subpath(beginIndex : Int, endIndex : Int) : CfsRelativePath = beginIndex match {
        case b if b <= parts.length || b >= endIndex || endIndex > parts.length ⇒
            throw new IllegalArgumentException(s"subpath bad indices: ($b, $endIndex)")
        case b ⇒ CfsRelativePath(parts.slice(b, endIndex))
    }

    /**
     * Tests if this path starts with a Path, constructed by converting the given path string,
     * in exactly the manner specified by the startsWith(Path) method. On UNIX for example,
     * the path "foo/bar" starts with "foo" and "foo/bar". It does not start with "f" or "fo".
     *
     * @param other the given path string
     * @return true if this path starts with the given path; otherwise false
     * @throws InvalidPathException - If the path string cannot be converted to a VPath
     */
    def startsWith(other : String) : Boolean = {
        CfsPathParser path (other, CfsRootRoot) match {
            case Full(path) ⇒ startsWith(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Tests if this path ends with a Path, constructed by converting the given path string,
     * in exactly the manner specified by the endsWith(VPath) method. On UNIX for example,
     * the path "foo/bar" ends with "foo/bar" and "bar". It does not end with "r" or "/bar".
     * Note that trailing separators are not taken into account, and so invoking this method
     * on the VPath "foo/bar" with the String "bar/" returns true.
     *
     * @param other the given path string
     * @return true if this path starts with the given path; otherwise false
     * @throws InvalidPathException - If the path string cannot be converted to a VPath
     */
    def endsWith(other : String) : Boolean = {
        CfsPathParser path (other, CfsRootRoot) match {
            case Full(path) ⇒ endsWith(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Converts a given path string to a Path and resolves it against this Path in exactly
     * the manner specified by the resolve method. For example, suppose that the name
     * separator is "/" and a path represents "foo/bar", then invoking this method with
     * the path string "gus" will result in the Path "foo/bar/gus".
     *
     * @param other the path string to resolve against this path
     * @return the resulting path
     * @throws InvalidPathException - if the path string cannot be converted to a VPath
     */
    def resolve(other : String) : VPath = {
        CfsPathParser path (other, CfsRootRoot) match {
            case Full(path) ⇒ resolve(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }

    /**
     * Resolves the given path against this path's parent path. This is useful where a
     * file name needs to be replaced with another file name. For example, suppose that
     * the name separator is "/" and a path represents "dir1/dir2/foo", then invoking this
     * method with the VPath "bar" will result in the VPath "dir1/dir2/bar". If this path
     * does not have a parent path, or other is absolute, then this method returns other.
     * If other is an empty path then this method returns this path's parent, or where
     * this path doesn't have a parent, the empty path.
     *
     * @param other the path to resolve against this path's parent
     * @return the resulting path
     */
    def resolveSibling(other : VPath) : VPath = getParent resolve other

    /**
     * Converts a given path string to a VPath and resolves it against this path's parent path
     * in exactly the manner specified by the resolveSibling method.
     *
     * @param other the path string to resolve against this path's parent
     * @return the resulting path
     * @throws InvalidPathException - if the path string cannot be converted to a VPath
     */
    def resolveSibling(other : String) : VPath = {
        CfsPathParser path other match {
            case Full(path) ⇒ resolveSibling(path)
            case _ ⇒ throw new InvalidPathException(other, "invalid Cfs path")
        }
    }
}

case class CfsRelativePath(parts : List[String]) extends CfsPath with CfsPathParts{
    /**
     * Tells whether or not this path is absolute.
     *
     * An absolute path is complete in that it doesn't need to be combined with other path
     * information in order to locate a file.
     *
     * @return true if, and only if, this path is absolute
     */
    def isAbsolute : Boolean = false

    /**
     * Returns the root component of this path as a VPath object, or null if this path does
     * not have a root component.
     *
     * @return a path representing the root component of this path, or null
     */
    def getRoot : CfsRootPath = null

    /**
     * Returns the parent path, or null if this path does not have a parent.
     *
     * The parent of this path object consists of this path's root component, if any, and
     * each element in the path except for the farthest from the root in the directory hierarchy.
     * This method does not access the file system; the path or its parent may not exist.
     * Furthermore, this method does not eliminate special names such as "." and ".." that
     * may be used in some implementations. On UNIX for example, the parent of "/a/b/c" is "/a/b",
     * and the parent of "x/y/." is "x/y". This method may be used with the normalize method, to
     * eliminate redundant names, for cases where shell-like navigation is required.
     *
     * If this path has one or more elements, and no root component, then this method is
     * equivalent to evaluating the expression:
     *
     * subpath(0, getNameCount()-1)
     *
     * @return a path representing the path's parent
     */
    def getParent : CfsRelativePath = CfsRelativePath(parts dropRight 1)

    /**
     * Tests if this path starts with the given path.
     *
     * This path starts with the given path if this path's root component starts with the root
     * component of the given path, and this path starts with the same name elements as the
     * given path. If the given path has more name elements than this path then false is returned.
     *
     * Whether or not the root component of this path starts with the root component of the given
     * path is file system specific. If this path does not have a root component and the given path
     * has a root component then this path does not start with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false is returned.
     *
     * @param other the given path
     * @return true if this path starts with the given path; otherwise false
     */
    def startsWith(other : VPath) : Boolean = other match {
        case rpath : CfsRelativePath ⇒ parts startsWith rpath.parts
        case _ ⇒ false
    }

    /**
     * Tests if this path ends with the given path.
     *
     * If the given path has N elements, and no root component, and this path has N or more
     * elements, then this path ends with the given path if the last N elements of each path,
     * starting at the element farthest from the root, are equal.
     *
     * If the given path has a root component then this path ends with the given path if the
     * root component of this path ends with the root component of the given path, and the
     * corresponding elements of both paths are equal. Whether or not the root component of
     * this path ends with the root component of the given path is file system specific. If
     * this path does not have a root component and the given path has a root component then
     * this path does not end with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false
     * is returned.
     *
     * @param other the given path
     * @return true if this path ends with the given path; otherwise false
     */
    def endsWith(other : VPath) : Boolean = other match {
        case relpath : CfsRelativePath ⇒ parts endsWith relpath.parts
        case _ ⇒ false
    }

    /**
     * Resolve the given path against this path.
     *
     * If the other parameter is an absolute path then this method trivially returns other.
     * If other is an empty path then this method trivially returns this path. Otherwise this
     * method considers this path to be a directory and resolves the given path against this
     * path. In the simplest case, the given path does not have a root component, in which
     * case this method joins the given path to this path and returns a resulting path that
     * ends with the given path. Where the given path has a root component then resolution
     * is highly implementation dependent and therefore unspecified.
     *
     * @param other the path to resolve against this path
     * @return the resulting path
     */
    def resolve(other : VPath) : VPath = {
        if (other.isAbsolute) other
        else other match {
            case relpath : CfsRelativePath ⇒ CfsRelativePath(parts ++ relpath.parts)
            case _ ⇒ other
        }
    }

    /**
     * Compares two abstract paths lexicographically. The ordering defined by this method
     * is provider specific, and in the case of the default provider, platform specific.
     * This method does not access the file system and neither file is required to exist.
     *
     * This method may not be used to compare paths that are associated with different
     * file system providers.
     *
     * @param other the path compared to this path.
     * @return zero if the argument is equal to this path, a value less than zero if this
     *         path is lexicographically less than the argument, or a value greater than
     *         zero if this path is lexicographically greater than the argument
     * @throws ClassCastException - if the paths are associated with different providers
     */
    def compareTo(other : VPath) : Int = other match {
        case relpath : CfsRelativePath ⇒ compareParts(parts, relpath.parts)
        case abspath : CfsAbsolutePath ⇒ compareParts(parts, abspath.root.parts ++ abspath.parts)
        case rootpath : CfsRootPath ⇒ compareParts(parts, rootpath.parts)
        case _ ⇒ throw new ClassCastException(s"CfsRelativePath compareTo ${other.getClass.getName}")
    }

    def /(name : String) : CfsRelativePath = {
        CfsPathParser filename name match {
            case Full(relpath) ⇒ CfsRelativePath(parts ++ relpath.parts)
            case _ ⇒ throw new InvalidPathException(name, "invalid Cfs name component")
        }
    }

    def resolve(cfspath : CfsPath) : CfsPath = cfspath match {
        case relpath : CfsRelativePath ⇒ CfsRelativePath(parts ++ relpath.parts)
        case other ⇒ other
    }

    def allParts : List[String] = parts

    override def toString : String = allParts.mkString("/")
}

object CfsEmptyPath extends CfsRelativePath(Nil) {

}


case class CfsAbsolutePath(root : CfsRootPath, parts : List[String])
    extends CfsPath with CfsPathParts {

    /**
     * Tells whether or not this path is absolute.
     *
     * An absolute path is complete in that it doesn't need to be combined with other path
     * information in order to locate a file.
     *
     * @return true if, and only if, this path is absolute
     */
    def isAbsolute : Boolean = true

    /**
     * Returns the root component of this path as a VPath object, or null if this path does
     * not have a root component.
     *
     * @return a path representing the root component of this path, or null
     */
    def getRoot : CfsRootPath = root

    /**
     * Returns the parent path, or null if this path does not have a parent.
     *
     * The parent of this path object consists of this path's root component, if any, and
     * each element in the path except for the farthest from the root in the directory hierarchy.
     * This method does not access the file system; the path or its parent may not exist.
     * Furthermore, this method does not eliminate special names such as "." and ".." that
     * may be used in some implementations. On UNIX for example, the parent of "/a/b/c" is "/a/b",
     * and the parent of "x/y/." is "x/y". This method may be used with the normalize method, to
     * eliminate redundant names, for cases where shell-like navigation is required.
     *
     * If this path has one or more elements, and no root component, then this method is
     * equivalent to evaluating the expression:
     *
     * subpath(0, getNameCount()-1)
     *
     * @return a path representing the path's parent
     */
    def getParent : CfsAbsolutePath = CfsAbsolutePath(root, parts dropRight 1)

    /**
     * Tests if this path starts with the given path.
     *
     * This path starts with the given path if this path's root component starts with the root
     * component of the given path, and this path starts with the same name elements as the
     * given path. If the given path has more name elements than this path then false is returned.
     *
     * Whether or not the root component of this path starts with the root component of the given
     * path is file system specific. If this path does not have a root component and the given path
     * has a root component then this path does not start with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false is returned.
     *
     * @param other the given path
     * @return true if this path starts with the given path; otherwise false
     */
    def startsWith(other : VPath) : Boolean = other match {
        case apath : CfsAbsolutePath ⇒ (root startsWith apath.root) && (parts startsWith apath.parts)
        case _ ⇒ false
    }

    /**
     * Tests if this path ends with the given path.
     *
     * If the given path has N elements, and no root component, and this path has N or more
     * elements, then this path ends with the given path if the last N elements of each path,
     * starting at the element farthest from the root, are equal.
     *
     * If the given path has a root component then this path ends with the given path if the
     * root component of this path ends with the root component of the given path, and the
     * corresponding elements of both paths are equal. Whether or not the root component of
     * this path ends with the root component of the given path is file system specific. If
     * this path does not have a root component and the given path has a root component then
     * this path does not end with the given path.
     *
     * If the given path is associated with a different FileSystem to this path then false
     * is returned.
     *
     * @param other the given path
     * @return true if this path ends with the given path; otherwise false
     */
    def endsWith(other : VPath) : Boolean = other match {
        case relpath : CfsRelativePath ⇒ parts endsWith relpath.parts
        case abspath : CfsAbsolutePath ⇒
            (root.parts endsWith abspath.root.parts) && parts == abspath.parts
        case rootpath : CfsRootPath ⇒ (root endsWith rootpath) && (parts == Nil)
        case _ ⇒ false
    }

    /**
     * Resolve the given path against this path.
     *
     * If the other parameter is an absolute path then this method trivially returns other.
     * If other is an empty path then this method trivially returns this path. Otherwise this
     * method considers this path to be a directory and resolves the given path against this
     * path. In the simplest case, the given path does not have a root component, in which
     * case this method joins the given path to this path and returns a resulting path that
     * ends with the given path. Where the given path has a root component then resolution
     * is highly implementation dependent and therefore unspecified.
     *
     * @param other the path to resolve against this path
     * @return the resulting path
     */
    def resolve(other : VPath) : VPath = {
        if (other.isAbsolute) other
        else other match {
            case relpath : CfsRelativePath ⇒ CfsAbsolutePath(root, parts ++ relpath.parts)
            case _ ⇒ other
        }
    }

    /**
     * Compares two abstract paths lexicographically. The ordering defined by this method
     * is provider specific, and in the case of the default provider, platform specific.
     * This method does not access the file system and neither file is required to exist.
     *
     * This method may not be used to compare paths that are associated with different
     * file system providers.
     *
     * @param other the path compared to this path.
     * @return zero if the argument is equal to this path, a value less than zero if this
     *         path is lexicographically less than the argument, or a value greater than
     *         zero if this path is lexicographically greater than the argument
     * @throws ClassCastException - if the paths are associated with different providers
     */
    def compareTo(other : VPath) : Int = other match {
        case rootpath : CfsRootPath ⇒ compareParts(root.parts ++ parts, rootpath.parts)
        case abspath : CfsAbsolutePath ⇒
            compareParts(root.parts ++ parts, abspath.root.parts ++ abspath.parts)
        case relpath : CfsRelativePath ⇒ compareParts(root.parts ++ parts, relpath.parts)
        case _ ⇒ throw new ClassCastException(s"CfsAbsPath compareTo ${other.getClass.getName}")
    }

    def resolve(cfspath : CfsPath) : CfsAbsolutePath = cfspath match {
        case relpath : CfsRelativePath ⇒ CfsAbsolutePath(root, parts ++ relpath.parts)
        case abspath : CfsAbsolutePath ⇒ abspath
        case rootpath : CfsRootPath ⇒ CfsAbsolutePath(rootpath, Nil)
    }

    def allParts : List[String] = root.allParts ++ parts

    def /(name : String) : CfsAbsolutePath = {
        CfsPathParser filename name match {
            case Full(relpath) ⇒ CfsAbsolutePath(root, parts ++ relpath.parts)
            case _ ⇒ throw new InvalidPathException(name, "invalid Cfs name component")
        }
    }

    override def toString : String = allParts.mkString("/", "/", "")
}

object CfsPath {

    def apply(path : String) : Box[CfsPath] = {
        CfsPathParser path (path, CfsRootRoot)
    }

    def apply(path : List[String]) : Box[CfsRelativePath] = {
        validateComponents (path) match {
            case Nil ⇒ Full(CfsRelativePath(path))
            case head :: _ ⇒ Failure(s"invalid Cfs path component '$head'")
        }
    }

    def apply(path : List[String], root : CfsRootPath) : Box[CfsAbsolutePath] = {
        validateComponents (path) match {
            case Nil ⇒ Full(CfsAbsolutePath(root, path))
            case head :: _ ⇒ Failure(s"invalid Cfs path component '$head'")
        }
    }

    def apply(path : List[String], base : CfsAbsolutePath) : Box[CfsAbsolutePath] = {
        validateComponents (path) match {
            case Nil ⇒ Full(CfsAbsolutePath(base.root, base.parts ++ path))
            case head :: _ ⇒ Failure(s"invalid Cfs path component '$head'")
        }
    }

    /**
     * Validate a list of path components. Returns Nil if all components are valid.
     * Otherwise returns the list of remaining components when an invalid component
     * was encountered, with the invalid component at the head.
     *
     * @param path list of path components
     * @return Nil if successful, otherwise see above
     */
    def validateComponents(path : List[String]) : List[String] = {
        path match {
            case Nil ⇒ Nil
            case head :: tail ⇒
                CfsPathParser filename head match {
                    case Full(_) ⇒ validateComponents(tail)
                    case _ : EmptyBox ⇒ path
                }
        }
    }
}