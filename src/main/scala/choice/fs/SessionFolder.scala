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
package choice.fs

import choice.model.{ResourceId, Resource}
import choice.access.{AnyRightsCheck, RightsCheck, Principal}
import net.liftweb.common._
import net.liftweb.mapper.BaseMetaMapper
import choice.core.Startup
import net.liftweb.common.Full
import choice.access.RightDef

/**
 * This is a container file which holds files related to a particular user session.
 * The file itself also has associated JSON-encoded data, which decodes into a
 * SessionSummary.
 */

abstract class SessionSummaryBase(val version : String)

case class SessionSummaryV1(startTime : Long,           // original session start time
                            endTime : Option[Long],     // session end time, if ended
                            attachTime : Option[Long],  // current activation start time, if active
                            detachTime : Option[Long],  // last deactivation time, if suspended
                            ipAddress : String,         // last client IP address
                            sessionId : String,         // last HTTP session id
                            userPath : String,          // path to user associated with session
                            userId : Long,              // userPath object resource id
                            userAgent : String          // last browser userAgent string
                            ) extends SessionSummaryBase("0.9.0")

case class SessionFolderNode(resource : Resource) extends CfsFnode(resource)
    with AtomicDataNode[SessionFolderNode] with JsonDataNode[SessionFolderNode, SessionSummaryV1] {
    val mf : Manifest[SessionSummaryV1] = manifest[SessionSummaryV1]

    /**
     * Create a file handle (VFile or its subclass) for a resource of this MIME type.
     *
     * @param path the filename path used to locate the file
     * @param principal the principal responsible for this open operation
     * @param options options that may affect the open operation
     * @return a boxed Library. A Failure may be returned for various errors, such as
     *         insufficient access rights, or unsupported options.
     */
    override def cfsOpen(path : CfsAbsolutePath, principal : Principal,
                         options : CfsOpenOptions) : Box[SessionFolder] = {
        Full(new SessionFolder(path, principal, this))
    }
}

class SessionFolder(path : CfsAbsolutePath, principal : Principal, vnode : SessionFolderNode)
    extends CfsFolder(path, principal, vnode)
    with JsonDataFile[SessionFolder, SessionFolderNode, SessionSummaryV1] {

    def getData : Box[SessionSummaryV1] = (SessionFolder canReadSummary this) { () ⇒ getDataUnchecked }

    def putData(data : SessionSummaryV1) : Box[Boolean] = (SessionFolder canWriteSummary this) { () ⇒
        putDataUnchecked (data)
    }
}

object SessionFolder extends CfsFolderHandler[SessionFolderNode, SessionFolder] {
    val Log = Logger("choice.fs.SessionFolder")

    override val getMimeType = "choice/session"

    override val name = "session"

    override def makeNode(resource : Resource) : Box[SessionFolderNode] = Full(SessionFolderNode(resource))

    override val getName = "Session File Type"

    override val getSchemas : List[BaseMetaMapper] = Nil

    //def makeFile(path : VPath, principal : Principal, vnode : N) : Box[F]
    def rights : List[RightDef] = List(
        RightDef("create_session", "session file", "create a session file"),
        RightDef("unlink_session", "session file", "unlink a session file"),
        RightDef("read_session", "session file", "read session summary"),
        RightDef("write_session", "session file", "write session summary"),
        RightDef("list_session", "session file", "list session data files"),
        RightDef("add_to_session", "session file", "add a session data file"),
        RightDef("remove_from_session", "session file", "remove a session data file")
    )

    def getSessionsFolder(principal : Principal) : CfsFolder = {
        Cfs open (Startup.SessionFolderPath, principal, CfsOpenOptions.Default) match {
            case Full(folder : CfsFolder) ⇒ folder
            case Full(other) ⇒
                other close ()
                sys.error(s"${Startup.SessionFolderPath} is not a folder")
            case e : EmptyBox ⇒ sys.error(s"""getSessionFolder: ${(e ?~ "(Empty)").msg}""")
        }
    }

    /**
     * Lookup a SessionFolder by its Resource id. The caller is responsible for closing
     * any returned file handle.
     *
     * @param sessionId the Resource id of the desired SessionFolder
     * @param principal the principal responsible for this operation
     * @return the boxed SessionFolder, Empty, or Failure
     */
    def getSessionById(sessionId : ResourceId, principal : Principal) : Box[SessionFolder] = {
        Cfs open (CfsVFileId(sessionId), principal, CfsOpenOptions.Default) match {
            case Full(sfolder : SessionFolder) ⇒ Full(sfolder)
            case Full(other) ⇒
                other close ()
                Failure(s"resource id $sessionId is not a SessionFolder")
            case e : EmptyBox ⇒ e
        }
    }

    /**
     * This defines the rights needed to list members of the container, or to reference
     * them by name.
     *
     * @return a RightsCheck instance
     */
    val canListMember : RightsCheck = AnyRightsCheck("list_session")

    /**
     * This defines the rights needed to add a member to the container, either by creating
     * a new file, or linking to an existing file.
     *
     * @return a RightsCheck instance
     */
    val canAddMember : RightsCheck = AnyRightsCheck("add_to_session")

    /**
     * This defines the rights needed to unlink a member from the container.
     *
     * @return a RightsCheck instance
     */
    val canRemoveMember : RightsCheck = AnyRightsCheck("remove_from_session")

    /**
     * This defines the rights needed to create a new instance of this object type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canCreateObject : RightsCheck = AnyRightsCheck("create_session")

    /**
     * This defines the rights needed to unlink an object of this type.
     * The principal must hold this right for the container in which the object is
     * to be created.
     *
     * @return a RightsCheck instance
     */
    val canUnlinkObject : RightsCheck = AnyRightsCheck("unlink_session")

    val canReadSummary = AnyRightsCheck("read_session")
    val canWriteSummary = AnyRightsCheck("write_session")
}