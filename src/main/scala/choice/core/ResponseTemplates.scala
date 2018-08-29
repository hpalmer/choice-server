/**
  * Copyright © 2015-2016 The Board of Trustees of The Leland Stanford Junior University.
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
  * This supports simple templates, mainly for error responses.
  *
  * @author Howard Palmer
  * Created by Hep on 6/12/2015.
  */
package choice.core

import choice.access._
import choice.actor.DbManager
import choice.fs._
import choice.parser.CfsPathParser
import net.liftweb.common.{EmptyBox, Full}
import net.liftweb.util.Helpers._

object ResponseTemplates {
    /**
     * Get the contents of a specified template file, substituting any keywords with values.
     * The keywords are processed in the order given. If there are keywords "foo" and "foobar",
     * it is important that "foobar" appears before "foo" in the sequence. In the file, keywords
     * are preceded by "$". The keyword names provided to this method do not contain the "$".
     *
     * @param filename the filename of the template file, which should be located in the system
     *                 templates folder
     * @param args a sequence of (keyword, value) pairs
     * @return
     */
    def getTemplateString(filename : String, args : Seq[(String, String)]) : String = {
        implicit val principal : () ⇒ CfsPrincipal = () ⇒ SystemPrincipal
        CfsPathParser abspath s"${Startup.TemplateFolderPath}/$filename" flatMap { path ⇒
            tryo(CfsFiles.newInputStream(path)) map { in ⇒
                val tstring = new String(readWholeStream(in))
                val resultString = (args foldLeft tstring) {
                    case (s, (argname, argvalue)) ⇒
                        s replaceAllLiterally (s"$$$argname", argvalue)
                }
                in close ()
                resultString
            }
        } openOr s"unable to access $filename"
    }

    def createDefaultTemplates() : Unit = {
        val folder = Startup.openOrCreateFolder(Startup.TemplateFolderPath, SystemPrincipal)
        folder getMember "404.html" match {
            case Full(vfile) ⇒ vfile close ()
            case _ : EmptyBox ⇒
                folder create ("404.html", "text/html", CfsCreateOptions.Default) match {
                    case Full(file) ⇒
                        val html =
                            """<!DOCTYPE html>
                               |<html>
                               |<head lang="en">
                               |    <meta charset="UTF-8">
                               |    <title>Link Not Found</title>
                               |</head>
                               |<body style="width:8in;font-size:14pt;margin-top:.75in;margin-left:1in;">
                               |<h1>Link Not Found</h1>
                               |<div>We don't recognize:</div>
                               |<div style="font-weight:bold;margin:20px .25in;">
                               |$url
                               |</div>
                               |<div>
                               |If you typed it in, perhaps you made a mistake. If you got
                               |here by clicking a link, then the person who made the link may have mistyped it.
                               |Or maybe &mdash; and I can't stress enough how unlikely this is &mdash; we made a mistake.
                               |</div>
                               |</body>
                               |</html>
                            """.stripMargin
                        val (dataSeqnum, _) = ChoiceData makeDataFile html.getBytes
                        file.getVnode withWriteLock { () ⇒
                            DbManager replaceData(file.getVnode, dataSeqnum, millis)
                        }
                        file close ()
                    case _ : EmptyBox ⇒
                        Startup.Log.error(s"unable to create 404.html in ${folder.getPath}")
                }
        }
        folder close ()
    }

}
