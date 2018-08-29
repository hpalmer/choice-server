/**
  * Copyright © 2014-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * Generic scripting support.
  *
  * @author Howard Palmer
  * Created by Hep on 9/8/2014.
  */
package choice.fs

import choice.access._
import net.liftweb.common.{Box, Logger}
import net.liftweb.json.JArray
import net.liftweb.mapper.BaseMetaMapper

/**
  * Common interface for script files.
  */
trait CfsScriptFile extends CfsFile {
    type ScriptResultType
    type ScriptOptionsType

    /**
     * Run the script as a specified principal. That is, any operations performed by the script
     * will use the access rights of the specified principal.
     *
     * @param principal the principal
     * @param args arguments for the script
     * @param options run options
     * @return a boxed value representing the result of the script, which generally should
     *         be representable as JSON
     */
    def run(principal : Principal, args : Option[JArray], options : ScriptOptionsType) : Box[ScriptResultType]

    /**
      * Run the script as the principal associated with the open script file handle. This
      * is the default way that scripts are run.
      *
      * @param args arguments for the script
      * @param options run options
      * @return a boxed value representing the result of the script, which generally should
      *         be representable as JSON
      */
    def runAsSelf(args : Option[JArray], options : ScriptOptionsType) : Box[ScriptResultType] = canRun(this) { () ⇒
        run(getPrincipal, args, options)
    }

    /**
      * Run the script using the owner of the script file as the principal. Among other things, this
      * can be used to have privileged scripts owned by an administrator.
      *
      * @param args arguments for the script
      * @param options run options
      * @return a boxed value representing the result of the script, which generally should
      *         be representable as JSON
      */
    def runAsOwner(args : Option[JArray],
                   options : ScriptOptionsType) : Box[ScriptResultType] = canRunAsOwner(this) { () ⇒
        val ownerId = getVnode.getOwnerId
        val owner = CfsPrincipal(ownerId)
        run(owner, args, options)
    }

    def canRun : RightsCheck
    def canRunAsOwner : RightsCheck
}

/**
 * Extend MimeTypeHandler for executable MIME types.
 */
trait ExecutableMimeType extends MimeTypeHandler {

    /**
      * Most files that have a MIME type handler are special. Mainly that means that their
      * MIME type should be immutable after they are created. CfsPlain files are generally
      * not special.
      *
      * So far all ExecutableMimeType files are plain, so they are not special.
      *
      * @return false by default
      */
    override def isSpecial_? : Boolean = false

    /**
     * This defines the rights needed to execute the file as the current user.
     *
     * @return a RightsCheck instance
     */
    val canRun : RightsCheck = AllRightsCheck("run_script")

    /**
     * This defines the rights needed to execute the file as the file's owner.
     *
     * @return a RightsCheck instance
     */
    val canRunAsOwner : RightsCheck = AllRightsCheck("run_as_owner")
}

object ExecutableMimeType extends Module {
    override val Log = Logger("choice.fs.ExecutableMimeType")
    override val getName = "ExecutableMimeType"

    override val getSchemas : List[BaseMetaMapper] = Nil

    override val getRights = List(
        RightDef("run_script", "executable files", "execute a script file"),
        RightDef("run_as_owner", "executable files", "execute a script file as the script's owner")
    )
}
