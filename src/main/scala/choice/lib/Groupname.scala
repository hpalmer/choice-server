/**
  * Copyright © 2011-2017 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.lib

import _root_.net.liftweb.common._
import util.parsing.combinator._

/**
 * Extractor for unqualified group name.
 * @author Howard Palmer
 *
 */
object Groupname {
    def unapply(groupname : Option[String]) : Option[String] = {
        getErrorMsg(groupname) match {
            case Empty ⇒ Some(groupname.get)
            case _ ⇒ None
        }
    }
    def getErrorMsg(groupname : Option[String]) : EmptyBox = {
        parseGroupname(groupname) match {
            case Full(gn) ⇒ Empty
            case f : Failure ⇒ f
            case Empty ⇒ Failure("group name is missing")
        }
    }
    def parseGroupname(gname : Option[String]) : Box[String] = {
        gname match {
            case None ⇒ Failure("group name is missing")
            case Some(gn) ⇒
                import GroupParser.{ Success, NoSuccess }
                GroupParser.parseAll(GroupParser.pathcomp, gn) match {
                    case Success(s, _) ⇒ Full(s)
                    case NoSuccess(msg, _) ⇒ Failure(msg)
                }
        }
    }
}

/**
 * Extractor object for an absolute or relative group path.
 */
object GroupPath {
    def unapply(path : Option[String]) : Option[List[String]] = {
        parseGroupPath(path) match {
            case Full(list) ⇒ Some(list)
            case _ ⇒ None
        }
    }
    def mkPath(path : List[String]) = {
        val p = path.mkString("/")
        if (p.startsWith("//")) p.substring(1) else p
    }
    def parseGroupPath(path : Option[String]) : Box[List[String]] = {
        path match {
            case Some(p) ⇒
                import PathParser.{ Success, NoSuccess }
                PathParser.parseAll(PathParser.path, p) match {
                    case Success(list, _) ⇒ Full(list)
                    case NoSuccess(msg, _) ⇒ Failure(msg)
                }
            case None ⇒ Failure("group name is missing")
        }
    }
}

/**
 * This parses a component of a group path.
 */
private object GroupParser extends RegexParsers {
    type T = String
    override def skipWhitespace = false
    def pathcomp = """\p{Alnum}[\w ,.#\-\(\)_]*""".r
}

/**
 * This parses an absolute or relative group path.
 */
private object PathParser extends RegexParsers {
    type T = List[String]
    override def skipWhitespace = false
    def root = "/"
    def repcomp = repsep(GroupParser.pathcomp, "/") <~ opt("/")
    def path = opt(root) ~ repcomp ^^ { x ⇒ if (x._1.isDefined) x._1.get :: x._2 else x._2 }
}
