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
  * @author Howard Palmer
  */
package choice.access

import choice.fs._
import choice.model.{ResourceId, Resource}
import net.liftweb.common._
import net.liftweb.mapper.BaseMetaMapper

/**
 * This is a special file type that defines an access right or permission.
 * Access rights are members of a container file known as a Role. Roles
 * are assigned to principals through Policy files.
 *
 * All the known access rights are contained in the folder defined by
 * 'rightsFolder'.
 */

class RightId(val id : Long) extends AnyVal

object RightId {
    import scala.language.implicitConversions
    implicit def rightIdToLong(rightId : RightId) : Long = rightId.id

    def apply(resid : ResourceId) : RightId = new RightId(resid.id)
}

class RightNode(resource : Resource) extends CfsSpecialNode(resource)
    with AtomicDataNode[RightNode]
    with JsonDataNode[RightNode, RightDef] {
    val mf : Manifest[RightDef] = manifest[RightDef]

    /**
     * Return true if this file is a container, i.e. supports the lookup() operation.
     * All Cfs files are containers, even if only for metadata files.
     *
     * @return true if this Vnode represents a container
     */
    override def isContainer_? : Boolean = false

    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[RightFile] = {
        Full(new RightFile (path, principal, this))
    }

    def getRightDef : RightDef = getData openOr sys.error(s"right object id ${getResourceId.id} is broken")
}

class RightFile(path : CfsAbsolutePath, principal : Principal, vnode : RightNode)
    extends CfsSpecial(path, principal, vnode)
    with JsonDataFile[RightFile, RightNode, RightDef] {

    override def getVnode : RightNode = vnode

    def getData : Box[RightDef] = (RightFile canReadDescription this) (() ⇒ getDataUnchecked)

    def putData(data : RightDef) : Box[Boolean] = (RightFile canWriteDescription this) { () ⇒
        putDataUnchecked (data) map (_ ⇒ true)
    }

    def update(desc : RightDef) : Box[RightFile] = {
        val name = getName
        (this putData desc) map (_ ⇒ this) or Failure(s"RightFile $name update failed")
    }

    def getRightDef : RightDef = getVnode.getRightDef
}

object RightFile extends MimeTypeHandler {
    import choice.core.Startup.RightsFolderPath

    override protected val Log = Logger("choice.access.RightFile")

    override def getName = "Right File Type"

    override def getSchemas : List[BaseMetaMapper] = Nil

    override def getMimeType = "choice/right"

    override def getRights = List(
        RightDef("create_right", "right file", "create a right"),
        RightDef("unlink_right", "right file", "unlink a right"),
        RightDef("read_right_desc", "right file", "read right description"),
        RightDef("write_right_desc", "right file", "write right description")
    )

    def apply(name : String, applicability : String, description : String, principal : Principal) : Box[RightId] = {
        val rfolder = getRightsFolder (principal)
        val mydesc = RightDef(name, applicability, description)
        val rfile = rfolder getMember name match {
            case Full(right : RightFile) ⇒
                right.getData match {
                    case Full(rdesc) ⇒
                        if (rdesc != mydesc) {
                            Log.warn(s"RightFile $name does not match existing file - attempting to update")
                            right update mydesc
                        }
                        else Full(right)
                    case Empty ⇒
                        Log.warn(s"RightFile $name is missing descriptor - attempting to update")
                        right update mydesc
                    case f : Failure ⇒
                        Log.error(s"RightFile $name descriptor read error - ${f.toString}")
                        right update mydesc
                }
            case Full(other) ⇒
                other close ()
                Failure(s"RightFile $name conflicts with existing non-right file")
            case Empty ⇒
                rfolder create (name, getMimeType, CfsCreateOptions.Default) match {
                    case Full(right : RightFile) ⇒ right update mydesc
                    case Full(other) ⇒
                        other close ()
                        Failure(s"RightFile $name creation return non-right file")
                    case e : EmptyBox ⇒ Failure(s"RightFile $name creation failed - ${e.toString}")
                }
            case f : Failure ⇒
                Log.error(s"RightFile $name error - ${f.toString}")
                f
        }
        rfolder close ()
        rfile map { rf ⇒
            val id = RightId(rf.getFileId.resource)
            rf.close()
            id
        }
    }

    def getRightId(name : String) : Box[RightId] = {
        Cfs.withValidPath(RightsFolderPath) { path ⇒
            Cfs.withExistingFile(path / name, SystemPrincipal, CfsOpenOptions.Default) {
                case rfile : RightFile ⇒ Full(RightId(rfile.getFileId.resource))
            }
        }
    }

    def getRightFile(name : String, principal : Principal) : Box[RightFile] = {
        val rfolder = getRightsFolder(principal)
        val result = rfolder open (name, principal, CfsOpenOptions.Default) match {
            case Full(rfile : RightFile) ⇒ Full(rfile)
            case Full(vfile) ⇒
                vfile close ()
                Failure(s"${rfolder.getPath} contains $name, which is not a right object")
            case e : EmptyBox ⇒ e
        }
        rfolder close ()
        result
    }

    def getRightsFolder(principal : Principal) : CfsFolder = {
        CfsPath(RightsFolderPath) flatMap { cfspath ⇒
            Cfs open (cfspath, principal, CfsOpenOptions.Default) match {
                case Full(rfolder : CfsFolder) ⇒ Full(rfolder)
                case Full(vfile) ⇒
                    vfile close ()
                    sys.error(s"$RightsFolderPath is not a folder")
                case Empty ⇒ sys.error(s"$RightsFolderPath does not exist")
                case f : Failure ⇒ sys.error(s"error opening $RightsFolderPath: ${f.msg}")
            }
        } openOr sys.error(s"$RightsFolderPath is not a valid Cfs path")
    }

    override def instantiate(resource : Resource) : Box[RightNode] = {
        if (isType_?(resource)) Full(new RightNode(resource))
        else Failure(s"resource id ${resource.getSafeKey.id} is not a right")
    }

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_right")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_right")

    val canReadDescription = AnyRightsCheck("read_right_desc")
    val canWriteDescription = AnyRightsCheck("write_right_desc")
}