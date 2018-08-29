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
  * Lua script file support.
  *
  * @author Howard Palmer
  */
package choice.fs

import choice.access.{Principal, RightDef, RightsCheck, SystemPrincipal}
import choice.model.Resource
import choice.script.lua.LuaLauncher
import net.liftweb.common._
import net.liftweb.json.JArray
import net.liftweb.mapper.BaseMetaMapper

class CfsLuaScriptNode(resource : Resource) extends CfsPlainNode(resource) {
    /**
     * Open the file associated with this Vnode. Mainly that means creating
     * some subclass of VFile that references this Vnode. Using the VFile,
     * the principal can perform various operations on the file. Depending
     * on the MIME type of the file, that may include acquiring input or
     * output streams, or performing other operations that are specific to
     * the file type.
     *
     * @param principal the principal to be associated with the VFile
     * @param options filesystem and file type specific options that may
     *                affect the state of the resulting VFile
     * @return a boxed VFile if successful, otherwise a Failure. Empty
     *         should not be returned.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[CfsPlain] = {
        Full(new CfsLuaScript(path, principal, this))
    }
}

case class LuaRunOptions()

/**
 * This represents a Lua script file, which is a subclass of CfsPlain. It mainly adds
 * a method to run the script.
 *
 * Created by Hep on 9/9/2014.
 */
class CfsLuaScript(path : CfsAbsolutePath, principal : Principal, vnode : CfsPlainNode)
    extends CfsPlain(path, principal, vnode) with CfsScriptFile {
    type ScriptResultType = (Int, Any)
    override type ScriptOptionsType = LuaRunOptions

    /**
     * Run the script as a specified principal. That is, any operations performed by the script
     * will use the access rights of the specified principal.
     *
     * @param principal the principal
     * @param args arguments for the script
     * @return a boxed value representing the result of the script, which generally should
     *         be representable as JSON
     */
    override def run(principal : Principal, args : Option[JArray], options : LuaRunOptions) : Box[(Int, Any)] = {
        // Open this file again using SystemPrincipal. At this point runAsSelf() or
        // runAsOwner() should have already checked that the principal associated with
        // this file handle is allowed to run the file. But LuaLauncher will get an
        // input stream, which requires read access, and the file handle principal
        // may not have that.
        Cfs.open(getPath, SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(script : CfsLuaScript) ⇒
                val result = LuaLauncher run (script, principal, args, options)
                if (!script.isClosed) script close ()
                result
            case Full(other) ⇒
                // This is highly unlikely
                other close ()
                Failure(s"script file changed type: ${getPath.toString}")
            case Empty ⇒ Failure(s"script file removed during run: ${getPath.toString}")
            case f : Failure ⇒ f
        }
    }

    override def canRunAsOwner : RightsCheck = CfsLuaScript.canRunAsOwner

    override def canRun : RightsCheck = CfsLuaScript.canRun
}

object CfsLuaScript extends ExecutableMimeType {
    override protected def Log : Logger = Logger("choice.fs.CfsLuaScript")

    override def getName : String = "Lua Script File"

    /**
     * Return a list of rights definitions for this module.
     *
     * The rights applicable to a CfsLuaScript file are defined by its superclass, CfsPlain,
     * and by the ExecutableMimeType module.
     *
     * @return list of rights definitions
     */
    override def getRights : List[RightDef] = Nil

    /**
     * Get a list of schemas used by this module, to be included in the Boot
     * schemify step.
     *
     * @return a list of Mapper objects representing database tables
     */
    override def getSchemas : List[BaseMetaMapper] = Nil

    /**
     * Get the string representation of the MIME type associated with this handler.
     */
    override def getMimeType : String = "text/x-lua"

    /**
     * Wrap a Resource object in an instance of the HandlerType for this MIME type handler.
     *
     * @param resource the Resource object as stored in the DB
     * @return if the resource is compatible with this MIME type, a boxed wrapper for the
     *         given resource, otherwise Failure
     */
    override def instantiate(resource : Resource) : Box[CfsPlainNode] = {
        if (isType_?(resource)) Full(new CfsLuaScriptNode(resource))
        else Failure(s"resource id ${resource.getSafeKey.id} is a special type")
    }

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container from which the object is
     * to be unlinked.
     *
     * @return a RightsCheck instance
     */
    override def canUnlinkObject : RightsCheck = CfsPlain.canUnlinkObject

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    override def canCreateObject : RightsCheck = CfsPlain.canCreateObject
}
