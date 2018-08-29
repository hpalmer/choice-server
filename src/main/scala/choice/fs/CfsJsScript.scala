/**
  * Copyright © 2016-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  * JavaScript script file support.
  *
  * @author Howard Palmer
  * Created by Hep on 10/5/2016.
  */
package choice.fs

import choice.access.{Principal, RightDef, RightsCheck, SystemPrincipal}
import choice.actor.DbManager
import choice.model.{MimeTypeId, Resource}
import choice.script.js.Launcher
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.mapper.BaseMetaMapper

class CfsJsScriptNode(resource : Resource) extends CfsPlainNode(resource) {
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
        Full(new CfsJsScript(path, principal, this))
    }
}

case class JsRunOptions(modules : Option[Boolean] = Some(false))

class CfsJsScript(path : CfsAbsolutePath, principal : Principal, vnode : CfsPlainNode)
    extends CfsPlain(path, principal, vnode) with CfsScriptFile {
    override type ScriptResultType = (Int, Any)
    override type ScriptOptionsType = JsRunOptions

    /**
      * Run the script as a specified principal. That is, any operations performed by the script
      * will use the access rights of the specified principal. This will close the script file.
      *
      * @param principal the principal
      * @param args arguments for the script
      * @return a boxed value representing the result of the script, which generally should
      *         be representable as JSON
      */
    override def run(principal : Principal, args : Option[JArray], options : JsRunOptions) : Box[(Int, Any)] = {
        // Open this file again using SystemPrincipal. At this point runAsSelf() or
        // runAsOwner() should have already checked that the principal associated with
        // this file handle is allowed to run the file. But Launcher will get an
        // input stream, which requires read access, and the file handle principal
        // may not have that.
        Cfs.open(getPath, SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(script : CfsJsScript) ⇒
                val result = Launcher run (script, principal, args, options)
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

object CfsJsScript extends ExecutableMimeType {
    override protected def Log : Logger = Logger("choice.fs.CfsJsScript")

    override def getName : String = "JavaScript Script File"

    /**
      * Return a list of rights definitions for this module.
      *
      * The rights applicable to a CfsJsScript file are defined by its superclass, CfsPlain,
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
    override def getMimeType : String = "text/javascript"

    /**
      * Override the normal isType_? to return true if the specified MIME type id is
      * any of those associated with this handler.
      *
      * @param mtid a MIME type id
      * @return true if mtid is one of ours
      */
    override def isType_?(mtid : MimeTypeId) : Boolean = allMimeTypeIds contains mtid

    /**
      * Override the normal isType_? to return true if the specified Resource has any
      * of the MIME types associated with this handler.
      *
      * @param resource a resource
      * @return true if the resource is managed by this handler
      */
    override def isType_?(resource : Resource) : Boolean = allMimeTypeIds contains resource.getMimeTypeId

    /**
      * Wrap a Resource object in an instance of the HandlerType for this MIME type handler.
      *
      * @param resource the Resource object as stored in the DB
      * @return if the resource is compatible with this MIME type, a boxed wrapper for the
      *         given resource, otherwise Failure
      */
    override def instantiate(resource : Resource) : Box[CfsPlainNode] = {
        if (isType_?(resource)) Full(new CfsJsScriptNode(resource))
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

    /**
      * Other JavaScript MIME types
      */
    val allMimeTypes : List[String] = List(
        "text/javascript",
        "application/javascript",
        "application/x-javascript",
        "application/ecmascript",
        "text/ecmascript"
    )

    allMimeTypes.tail foreach (s ⇒ DbManager registerMimeTypeHandler(this, s))

    /**
      * Delay the construction of this list until after DbManager has been initialized.
      */
    lazy val allMimeTypeIds : List[MimeTypeId] = {
        allMimeTypes map (s ⇒ DbManager getMimeTypeId s)
    }
}
