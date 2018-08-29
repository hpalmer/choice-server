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
  * Global configuration parameters for the Choice infrastructure. This
  * maps to a database table that always has exactly one entry.
  *
  * @author Howard Palmer
  *
  */
package choice.model

import choice.access.{BootPrincipal, Principal, SystemPrincipal}
import choice.actor.DbManager
import choice.core.Startup
import choice.fs._
import net.liftweb.common._

object GlobalConfig {
    private val Log = Logger("choice.model.GlobalConfig")

    val guestUserPath = "/System/Users/guest"
    val anyoneGroupPath = "/System/Users/anyone"

    /**
     * Get the root of the filesystem. If it does not exist, DbManager will create it.
     * Use a special bootstrap principal, because there may not be any other
     * principals in existence yet.
     *
     * @param by    the principal for this operation, defaulting to the BootPrincipal
     * @return a CfsFolder for the filesystem root
     */
    def getRoot(by : Principal = BootPrincipal) : CfsFolder = Cfs open (CfsRootPath, by, CfsOpenOptions.Default) match {
        case Full(folder : CfsFolder) ⇒ folder
        case Full(_) ⇒ sys.error("filesystem root is not a folder")
        case Empty ⇒ sys.error("filesystem root open error")
        case f : Failure ⇒ sys.error(f.toString)
    }

    /**
     * Get the system folder.
     *
     * @param by    the principal for this operation, defaulting to the BootPrincipal
     * @return a CfsFolder for the system folder, or Failure
     */
    def getSystemFolder(by : Principal = BootPrincipal) : CfsFolder = {
        import Startup.SystemFolderPath
        Cfs.withValidPath(SystemFolderPath) { cfspath ⇒
            Cfs open (cfspath, SystemPrincipal, CfsOpenOptions.Default) match {
                case Full(folder : CfsFolder) ⇒ Full(folder)
                case Full(vfile) ⇒
                    vfile close ()
                    sys.error(s"$SystemFolderPath is not a folder")
                case Empty ⇒ sys.error(s"$SystemFolderPath does not exist")
                case f : Failure ⇒ sys.error(s"error opening $SystemFolderPath: ${f.msg}")
            }
        } openOr sys.error(s"invalid Cfs path $SystemFolderPath")
    }

    /**
     * Get the default user group.
     *
     * @param by    the principal for this operation, defaulting to the BootPrincipal
     * @return a GroupInfo for the default user group, or Failure
     */
    def getDefaultGroup(by : Principal = BootPrincipal) : GroupInfo = {
        import Startup.DefaultUsersPath
        Cfs.withValidPath(DefaultUsersPath) { cfspath ⇒
            Cfs open (cfspath, SystemPrincipal, CfsOpenOptions.Default) match {
                case Full(ginfo : GroupInfo) ⇒ Full(ginfo)
                case Full(vfile) ⇒
                    vfile close ()
                    sys.error(s"$DefaultUsersPath is not a user group")
                case Empty ⇒ sys.error(s"$DefaultUsersPath does not exist")
                case f : Failure ⇒ sys.error(s"error opening $DefaultUsersPath: ${f.msg}")
            }
        } openOr sys.error(s"invalid Cfs path: $DefaultUsersPath")
    }

    /**
     * Get the system administrators group.
     *
     * @param by    the principal for this operation, defaulting to the BootPrincipal
     * @return a GroupInfo for the administrators group
     */
    def getAdminGroup(by : Principal = BootPrincipal) : GroupInfo = {
        import Startup.AdminUsersPath
        Cfs open (AdminUsersPath, SystemPrincipal, CfsOpenOptions.Default) match {
            case Full(ginfo : GroupInfo) ⇒ ginfo
            case Full(vfile) ⇒
                vfile close ()
                sys.error(s"$AdminUsersPath is not a user group")
            case Empty ⇒ sys.error(s"$AdminUsersPath does not exist")
            case f : Failure ⇒ sys.error(s"error opening $AdminUsersPath: ${f.msg}")
        }
    }

    /**
     * Check whether the first administrator has registered yet.
     * 
     * @return true if there is at least one user in the Admin group
     */
    def haveFirstAdmin : Boolean = {
        import Startup.AdminUsersPath
        Cfs.withExistingFile (AdminUsersPath, BootPrincipal, CfsOpenOptions.Default) {
            case ginfo : GroupInfo ⇒
                val adminUsers = ginfo.getUsers
                val result = adminUsers.length
                adminUsers foreach (_ close ())
                Log.info(s"there are $result administrators")
                Full(result > 1)
        } openOr false
    }

    private val adminCache = collection.mutable.Map[ResourceId, Boolean]()

    def clearAdminCache() : Unit = {
        adminCache.clear()
    }

    def clearAdminCache(pid : ResourceId) : Unit = {
        adminCache.remove(pid)
    }

    def isSystemAdmin_?(principal : Principal) : Boolean = {
        adminCache.getOrElse (principal.getPrincipalId, {
            val isadmin = (principal eq BootPrincipal) || (principal eq SystemPrincipal) || {
                val admingroup = getAdminGroup()
                val namebox = DbManager getMemberName(admingroup.getFileId.resource, principal.getPrincipalId)
                admingroup close()
                namebox map (_ ⇒ true) openOr false
            }
            adminCache put (principal.getPrincipalId, isadmin)
            isadmin
        })
    }

    def isSystemAdmin_?(uinfo : UserInfo) : Boolean = isSystemAdmin_?(uinfo.getSelfPrincipal)
}
