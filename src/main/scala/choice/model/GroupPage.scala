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
  * A designated page for a group.
  * 
  * @author Howard Palmer
  *
  */
package choice.model

import choice.fs.GroupInfo
import choice.model.PageType._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

class GroupPage extends LongKeyedMapper[GroupPage] with IdPK {
    def getSingleton : GroupPage.type = GroupPage

    object group extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }

    object page extends MappedLongForeignKey(this, PageDef) {
        override def dbNotNull_? = true
    }

    object ptype extends MappedEnum(this, PageType) {
        override def dbNotNull_? = true
    }

}

object GroupPage extends GroupPage with LongKeyedMetaMapper[GroupPage] {
    val defaultHomePage = "group_default.html"

    def getPage(group : GroupInfo, ptype : PageType) : Box[PageDef] = {
        find(By(GroupPage.group, group.getFileId.resource), By(GroupPage.ptype, ptype)) match {
            case Full(gp) ⇒ gp.page.obj
            case f : Failure ⇒ f
            case Empty ⇒ Empty
        }
    }

    def setPage(group : GroupInfo, ptype : PageType, page : Box[PageDef]) : Box[GroupPage] = {
        val gid = group.getFileId.resource
        find(By(GroupPage.group, gid), By(GroupPage.ptype, ptype)) foreach (_.delete_!)
        page map { pdef ⇒ GroupPage.create.group(gid).page(pdef).ptype(ptype).saveMe() }
    }

    def getGuestPage(group : GroupInfo) : Box[PageDef] = getPage(group, GUESTPAGE)

    def getHomePage(group : GroupInfo) : Box[PageDef] = getPage(group, HOMEPAGE)

    def getDefaultHomePage : Box[PageDef] = {
        PageDef.find(By(PageDef.page, defaultHomePage)) match {
            case pdb @ Full(_) ⇒ pdb
            case f : Failure ⇒ f
            case Empty ⇒
                tryo(PageDef.create.page(defaultHomePage).saveMe()) ?~!
                    "failed to initialize default home page"

        }
    }
}