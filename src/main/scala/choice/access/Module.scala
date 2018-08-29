/**
  * Copyright © 2013-2018 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.access

import net.liftweb.mapper.BaseMetaMapper
import net.liftweb.common._
import choice.fs.CfsFile
import net.liftweb.common.Full
import choice.fs.vfs.CanHazMap

/**
 * This defines an interface to a module that is initialized at server startup.
 * It provides an entry point for getting database schemas used by the module,
 * which are included in the Boot schemify step. It provides another entry
 * point, which is called after schemify, that can do things such as define
 * MIME types, access rights, and various system files.
 *
 * Other entry points and support for loadable modules may eventually be added.
 */
trait Module {

    protected def Log : Logger

    def getName : String

    /**
     * Get a list of schemas used by this module, to be included in the Boot
     * schemify step.
     *
     * @return a list of Mapper objects representing database tables
     */
    def getSchemas : List[BaseMetaMapper]

    /**
     * Return a list of rights definitions for this module.
     *
     * @return list of rights definitions
     */
    def getRights : List[RightDef]

    /**
     * Initialize this module. This is called immediately after the Boot schemify
     * step.
     */
    def init() : Unit = {
        Module defineRights getRights
        getRights foreach { rdef ⇒
            val id = ((Module getRightId rdef.name) fold "FAILED") { _.id.toString }
            Log.info(s"    Right ${rdef.name} → id $id")
        }
        Log.info(s"Module '$getName' initialized")
    }
}

object Module {

    private val Log = Logger("choice.core.Module")

    import collection.mutable.{Map ⇒ MutableMap}
    val rightsById : MutableMap[RightId, RightDef] = MutableMap[RightId, RightDef]()
    val rightsByName : MutableMap[String, RightId] = MutableMap[String, RightId]()

    def defineRights(rights : List[RightDef]) : Map[String, RightId] = {
        rights foreach { rdef ⇒
            RightFile(rdef.name, rdef.applicability, rdef.description, SystemPrincipal) match {
                case Full(rid) ⇒
                    rightsById put (rid, rdef)
                    rightsByName put (rdef.name, rid)
                case e : EmptyBox ⇒
                    Log.error(s"error creating file for right ${rdef.name}", e)
            }
        }
        rightsByName.toMap
    }

    def getRightId(name : String) : Option[RightId] = rightsByName get name
    def getRightDef(name : String) : Option[RightDef] = rightsByName get name flatMap rightsById.get
    def getRightDef(id : RightId) : Option[RightDef] = rightsById get id
}

/**
 * Define an access right. A module may define a set of access rights,
 * which control access to the operations implemented by the module.
 *
 * @param name a short name, by which users generally reference this right
 * @param applicability when the right applies
 * @param description a description of what operations(s) this right permits
 */
case class RightDef(name : String, applicability : String, description : String) extends CanHazMap {
    override def asMap : Map[String, Any] = Map("name" → name,
                                                "applicability" → applicability,
                                                "description" → description)
}

abstract class RightsCheck(rights : Seq[String]) {
    protected lazy val _rightIds : Seq[RightId] = rightNamesToIds(rights)
    def Log : Logger
    def rightIds : Seq[RightId] = _rightIds

    def apply[F <: CfsFile, R](file : F)(f : () ⇒ Box[R]) : Box[R] = apply(file, file.getPrincipal)(f)

    def apply[F <: CfsFile, R](file : F, principal : Principal)(f : () ⇒ Box[R]) : Box[R] = {
        if (checkRights(file, principal)) f()
        else if (principal eq GuestPrincipal) AuthNeededFailure
        else Failure("access denied")
    }

    def checkRights[F <: CfsFile](file : F, principal : Principal) : Boolean

    def rightNamesToIds(rights : Seq[String]) : Seq[RightId] = {
        rights flatMap { name ⇒
            val result = Module getRightId name
            result match {
                case Some(_) ⇒
                case None ⇒ Log.error(s"no id for $name")
            }
            result
        }
    }
}

class AnyRightsCheck(rights : Seq[String]) extends RightsCheck(rights) {
    val Log : Logger = AnyRightsCheck.Log
    //Log.debug(s"rights.length = ${rights.length}: ${rights.mkString(",")}")

    def checkRights[F <: CfsFile](file : F, principal : Principal) : Boolean = file hasAnyRights (rightIds, principal)
}

object AnyRightsCheck {
    val Log = Logger("choice.core.AnyRightsCheck")
    def apply(rights : String *) = new AnyRightsCheck(rights)
}

class AllRightsCheck(rights : Seq[String]) extends RightsCheck(rights) {
    val Log : Logger = AllRightsCheck.Log
    //Log.debug(s"rights.length = ${rights.length}: ${rights.mkString(",")}")

    def checkRights[F <: CfsFile](file : F, principal : Principal) : Boolean = file hasAllRights (rightIds, principal)
}

object AllRightsCheck {
    val Log = Logger("choice.core.AllRightsCheck")
    def apply(rights : String *) = new AllRightsCheck(rights)
}
