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
package choice.parser

import choice.fs.{CfsPath, CfsRelativePath, CfsRootRoot}
import choice.core.{ComponentDescriptor, Route, RoutePath}

import scala.util.matching.Regex

/**
 * Parse a component description into a Component.
 */
object CompParser extends CfsPathParser {

    override val whiteSpace : Regex = """[ \t]+""".r
    override val skipWhitespace = false

    def ws : Parser[Any] = whiteSpace

    def eol : Regex = """\r?\n""".r

    def line_comment : Parser[Any] = """//[^\r\n]*""".r <~ eol

    def end : Parser[Any] = opt(ws) ~ (line_comment | eol)

    def comments : Parser[Any] = rep(end)

    def group_statement : Parser[CfsPath] = (opt(ws) ~ "group" ~ ws) ~> path(CfsRootRoot) <~ end

    def empty_relpath : Parser[CfsRelativePath] = "." ^^ (_ => CfsRelativePath(Nil))

    def route_source : Parser[CfsRelativePath] = empty_relpath | (relpath ^^ (list => CfsRelativePath(list)))

    def route_target : Parser[CfsPath] = empty_relpath | path(CfsRootRoot)

    def arrow : Parser[Any] = opt(ws) ~ "=>" ~ opt(ws)

    def route_statement : Parser[Route] =
        ((opt(ws) ~ "route" ~ ws) ~> (route_source <~ arrow) ~ route_target <~ end) ^^ {
            case source ~ target =>
                val urlParts = source.allParts
                val routeParts = target.allParts
                Route(urlParts, RoutePath(routeParts, target.isAbsolute))
        }

    def head_section : Parser[Option[CfsPath]] = comments ~> opt(group_statement)

    def route_section : Parser[List[Route]] = comments ~> rep1sep(route_statement, comments)

    def component : Parser[ComponentDescriptor] =
        head_section ~ route_section <~ comments ^^ {
            case optgroup ~ routelist =>
                ComponentDescriptor(routelist, optgroup map (_.toString))
        }
}
