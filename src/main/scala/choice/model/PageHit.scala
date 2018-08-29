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
  * @author Howard Palmer
  */
package choice.model

import net.liftweb.mapper._
import scala.xml._
import java.text.DateFormat
import java.util.Date

class PageHit extends LongKeyedMapper[PageHit] with IdPK {
    def getSingleton : PageHit.type = PageHit
	final val dateFormat =
		DateFormat.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG)
    object sessionId extends MappedLongForeignKey(this, Resource) {
        override def dbNotNull_? = true
    }
    // Server timestamp
    object hitTime extends MappedLong(this) {
        override def dbNotNull_? = true
    	override def asHtml = Text(dateFormat.format(new Date(get)))
    }
    object principal extends MappedLongForeignKey(this, Resource) {
      override def dbNotNull_? = true
    }
    object page extends MappedLongForeignKey(this, PageDef) {
        override def dbNotNull_? = true
    }
}

object PageHit extends PageHit with LongKeyedMetaMapper[PageHit]