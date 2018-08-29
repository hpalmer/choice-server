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
  * Parsers for file paths in the Choice File System.
  *
  * @author Howard Palmer
  */
package choice.parser

import java.nio.file.InvalidPathException

import choice.fs._
import net.liftweb.common
import net.liftweb.common.{Box, Full}

import scala.util.parsing.combinator.RegexParsers

/**
 * Support for parsing Cfs file paths.
 */

trait CfsPathParser extends RegexParsers {
    /** Parser for a single filename component */
    def filename : Parser[String] = """[\p{Alnum}._$@\-][\w ,.$@#\-\(\)_~]*[\w,.$@#\-\(\)_~]""".r |
                                    """[\p{Alnum}._$@\-]""".r

    def relpath : Parser[List[String]] = rep1sep(filename, "/") <~ opt("/")

    def abspath : Parser[List[String]] = "/" ~> relpath | "/" ~> repsep(filename, "/")

    def path(root : CfsRootPath) : Parser[CfsPath] = abspath ^^ { list : List[String] => CfsAbsolutePath(root, list) } |
        relpath ^^ { list : List[String] => CfsRelativePath(list) }
}

object CfsPathParser extends CfsPathParser {
    override def skipWhitespace = false

    /**
     * The CfsPath constructors throw exceptions. This wrapper function catches them
     * and turns them into Failures.
     *
     * @param f the function using CfsPath to be wrapped
     * @tparam T the boxed type returned by f
     * @return the result of f, or an exception boxed as a Failure
     */
    def catchPathExceptions[T](f : () => Box[T]) : Box[T] = {
        try f() catch {
            case e : InvalidPathException => common.Failure(s"${e.getMessage}: ${e.getReason}")
            case e : IllegalArgumentException => common.Failure(s"$e")
            case e : ClassCastException => common.Failure(s"$e")
        }
    }

    /**
     * Parse a single filename component, and return as a relative CfsPath.
     *
     * @param input the input to be parsed completely
     * @return a boxed, relative CfsPath if successful, otherwise Failure
     */
    def filename(input : String) : Box[CfsRelativePath] = {
        val myfilename : Parser[String] = filename
        catchPathExceptions { () =>
            parseAll(myfilename, input) match {
                case Success(result, _) => Full(CfsRelativePath(List(result)))
                case failure : NoSuccess => common.Failure(failure.msg)
            }
        }
    }

    /**
     * Attempt to parse a single Cfs filename component, returning a relative
     * CfsPath if successful.
     *
     * @param input the input to be parsed, not necessarily completely
     * @return a ParseResult of Success with a CfsRelativePath if successful,
     *         otherwise a NoSuccess result. In both cases the unmatched input
     *         is part of the ParseResult.
     */
    def filenamePrefix(input : Input) : ParseResult[CfsRelativePath] = {
        val myfilename : Parser[String] = filename
        parse(myfilename, input) match {
            case Success(result, next) => Success(CfsRelativePath(List(result)), next)
            case fail : NoSuccess => fail
        }
    }

    /**
     * Parse a relative file path. It must not start with "/", but it can end
     * with "/".
     *
     * @param input the input to be parsed completely
     * @return a boxed CfsRelativePath if successful, otherwise Failure
     */
    def relpath(input : String) : Box[CfsRelativePath] = {
        val myrelpath : Parser[List[String]] = relpath
        catchPathExceptions { () =>
            parseAll(myrelpath, input) match {
                case Success(result, _) => Full(CfsRelativePath(result))
                case failure : NoSuccess => common.Failure(failure.msg)
            }
        }
    }

    def relpathPrefix(input : Input) : ParseResult[CfsRelativePath] = {
        val myrelpath : Parser[List[String]] = relpath
        parse(myrelpath, input) match {
            case Success(result, next) => Success(CfsRelativePath(result), next)
            case fail : NoSuccess => fail
        }
    }

    /**
     * Parse an absolute file path. It must start with "/", and may end with "/",
     * provided there is at least one name component.
     *
     * @param input the input to be parsed completely
     * @param root a root path, defaulting to the standard root
     * @return a boxed CfsAbsolutePath if successful, otherwise Failure
     */
    def abspath(input : String, root : CfsRootPath = CfsRootRoot) : Box[CfsAbsolutePath] = {
        val myabspath : Parser[List[String]] = abspath
        catchPathExceptions { () =>
            parseAll(myabspath, input) match {
                case Success(result, _) => Full(CfsAbsolutePath(root, result))
                case failure : NoSuccess => common.Failure(failure.msg)
            }
        }
    }

    def abspathPrefix(input : Input) : ParseResult[CfsAbsolutePath] = {
        val myabspath : Parser[List[String]] = abspath
        parse(myabspath, input) match {
            case Success(result, next) => Success(CfsAbsolutePath(CfsRootRoot, result), next)
            case fail : NoSuccess => fail
        }
    }

    /**
     * Parse a general file path, which may be relative or absolute.
     *
     * @param input the input to be parsed completely
     * @param root a root path, defaulting to the standard root
     * @return a boxed CfsPath if successful, otherwise Failure
     */
    def path(input : String, root : CfsRootPath = CfsRootRoot) : Box[CfsPath] = {
        val mypath : Parser[CfsPath] = path(root)
        catchPathExceptions { () =>
            parseAll(mypath, input) match {
                case Success(result, _) => Full(result)
                case failure : NoSuccess => common.Failure(failure.msg)
            }
        }
    }

    def pathPrefix(input : Input, root : CfsRootPath = CfsRootRoot) : ParseResult[CfsPath] = {
        val mypath : Parser[CfsPath] = path(root)
        parse(mypath, input)
    }
}
