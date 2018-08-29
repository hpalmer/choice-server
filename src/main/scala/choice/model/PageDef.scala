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
  * This represents a unique page in the site. Each page may have multiple
  * associated PageHit objects, recording hits on the page.
  *
  * @author Howard Palmer
  */
package choice.model

import net.liftweb.mapper._

class PageDef extends LongKeyedMapper[PageDef] with OneToMany[Long, PageDef] with IdPK {
    def getSingleton : PageDef.type = PageDef
    object page extends MappedString(this, 255) {
        override def dbNotNull_? = true
    }
    object pageHits extends MappedOneToMany(PageHit, PageHit.page,
    										OrderBy(PageHit.hitTime, Ascending))
}

object PageDef extends PageDef with LongKeyedMetaMapper[PageDef] {
    override def dbIndexes : List[BaseIndex[PageDef]] = UniqueIndex(page) :: super.dbIndexes
}
