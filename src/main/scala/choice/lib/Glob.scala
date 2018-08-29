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
  * Convert shell globbing pattern to a regular expression.
  *
  * @author Howard Palmer
  */
package choice.lib

import scala.util.matching.Regex
import net.liftweb.common.Full
import net.liftweb.util.Helpers.tryo

object Glob {

    /**
     * Converts a standard POSIX Shell globbing pattern into a regular expression
     * pattern. The result can be used with the standard `` API to
     * recognize strings which match the glob pattern.
     * <p/>
     * See also, the POSIX Shell language:
     * http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_13_01
     *
     * @param glob A glob pattern.
     * @return A regex pattern to recognize the given glob pattern.
     */
    def globToRegex(glob : String) : String = {
        val sb = new StringBuilder
        def helper(s : List[Char], inGroup : Int, inClass : Int,
                   firstInClass : Boolean, pathSepLast : Boolean) : String = {
            s match {
                case Nil ⇒ (sb append '$').toString()
                case ch :: rest ⇒ ch match {
                    case '\\' ⇒
                        rest match {
                            case Nil ⇒
                                sb append '\\'
                                sb.toString()
                            case next :: more ⇒
                                next match {
                                    case ',' ⇒
                                    case 'Q' | 'E' ⇒ sb append "\\\\"
                                    case _ ⇒ sb append '\\'
                                }
                                sb append next
                                helper(more, inGroup, inClass, firstInClass = false, pathSepLast = false)
                        }
                    case '*' ⇒
                        rest match {
                            case Nil ⇒
                                sb append (if (inClass == 0) "[^/]*" else "*")
                                helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                            case next :: more ⇒
                                next match {
                                    case '*' ⇒
                                        if (pathSepLast && more.headOption.contains('/')) {
                                            sb append "(.*?/)*?"
                                            helper(more.tail, inGroup, inClass, firstInClass = false, pathSepLast = true)
                                        }
                                        else {
                                            sb append (if (inClass == 0) ".*?" else "*")
                                            helper(more, inGroup, inClass, firstInClass = false, pathSepLast = false)
                                        }
                                    case _ ⇒
                                        sb append (if (inClass == 0) "[^/]*" else "*")
                                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                                }
                        }
                    case '/' ⇒
                        sb append '/'
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = true)
                    case '?' ⇒
                        sb append (if (inClass == 0) "[^/]" else "?")
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                    case '[' ⇒
                        sb append '['
                        helper(rest, inGroup, inClass + 1, firstInClass = true, pathSepLast = false)
                    case ']' ⇒
                        sb append ']'
                        helper(rest, inGroup, inClass - 1, firstInClass = false, pathSepLast = false)
                    case '.' | '(' | ')' | '+' | '|' | '^' | '$' | '@' | '%' ⇒
                        if ((inClass == 0) | (firstInClass && (ch == '^'))) sb append '\\'
                        sb append ch
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                    case '!' ⇒
                        sb append (if (firstInClass) '^' else '!')
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                    case '{' ⇒
                        sb append '('
                        helper(rest, inGroup + 1, inClass, firstInClass = false, pathSepLast = false)
                    case '}' ⇒
                        sb append ')'
                        helper(rest, inGroup - 1, inClass, firstInClass = false, pathSepLast = false)
                    case ',' ⇒
                        sb append (if (inGroup > 0) '|' else ',')
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)
                    case _ ⇒
                        sb append ch
                        helper(rest, inGroup, inClass, firstInClass = false, pathSepLast = false)

                }
            }
        }
        helper(glob.toList, 0, 0, firstInClass = false, pathSepLast = false)
    }

    def matches(glob : String, s : String) : Boolean = {
        tryo(new Regex(globToRegex(glob))) match {
            case Full(reglob) ⇒
//                printf(s"reglob = $reglob\n")
                reglob findPrefixOf s match {
                    case None ⇒ false
                    case Some(ms) ⇒
//                        printf(s"matched '$ms'\n")
                        ms.length == s.length
                }
            case _ ⇒ false
        }
    }
}
