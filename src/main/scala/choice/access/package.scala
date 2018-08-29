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
package choice

import choice.core.Startup
import choice.fs.{CfsOpenOptions, Cfs, GroupInfo, UserInfo}
import choice.model.ResourceId
import net.liftweb.common.{Failure, Full}

/**
 * Auxiliary definitions for access control API.
 */
package object access {

    /**
     * Failure returned when authentication is required to access an object. This does
     * not imply that all authenticated users will necessarily be granted access, only
     * that the current principal is not authenticated and that no unauthenticated
     * principals will have access.
     */
    val AuthNeededFailure = Failure("authentication required")

    /**
     * Failure returned when access is denied, other than in cases where AuthNeededFailure
     * applies.
     */
    val AccessDeniedFailed = Failure("access denied")

    /**
     * This is a special principal used to bootstrap access when no other principal
     * is available.
     */
    object BootPrincipal extends Principal with IsPrincipal with HasPrincipal {
        override val getPrincipalId : ResourceId = ResourceId(1L)
        override val getParents : List[Principal] = Nil
        override val getPrincipalName : String = "System"
        override val isGroup_? = false
        override val isSystemAdmin_? : Boolean = true
        override val getPrincipal : Principal = this
        override val getSelfPrincipal : Principal = this
        override val getAncestors : Stream[Principal] = Stream(this)
    }

    /**
     * This principal is used for internal operations for which no other principal
     * is readily available.
     */
    lazy val SystemPrincipal : CfsPrincipal =
        Cfs.withExistingFile (Startup.SystemUserPath, BootPrincipal, CfsOpenOptions.Default) {
            case sysuser : UserInfo => Full(sysuser.getSelfPrincipal)
        } openOr sys.error("SystemPrincipal initialization failed")

    /**
     * This principal is a group to which all authenticated users belong, at least for
     * purposes of access control.
     */
    lazy val AnyonePrincipal : CfsPrincipal =
        Cfs.withExistingFile (Startup.AnyoneGroupPath, BootPrincipal, CfsOpenOptions.Default) {
            case anyone : GroupInfo => Full(anyone.getSelfPrincipal)
        } openOr sys.error("AnyonePrincipal initialization failed")

    /**
     * This principal is used to represent an unauthenticated user. Everyone is Guest
     * before they login.
     */
    lazy val GuestPrincipal : CfsPrincipal =
        Cfs.withExistingFile (Startup.GuestUserPath, BootPrincipal, CfsOpenOptions.Default) {
            case guest : UserInfo => Full(guest.getSelfPrincipal)
        } openOr sys.error("GuestPrincipal initialization failed")
}
