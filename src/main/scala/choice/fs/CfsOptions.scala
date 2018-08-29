/**
  * Copyright © 2015-2016 The Board of Trustees of The Leland Stanford Junior University.
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

import net.liftweb.util.Helpers.millis

/**
 * Containers for options used in Cfs calls.
 *
 * Created by Hep on 5/25/2015.
 */

/**
 * Base class common to options types.
 */
class CfsOptions

/**
 * Base class for file creation options. Specific file types may subclass this.
 *
 * @param dataSeqnum the sequence number of a ChoiceData file containing the data for the
 *                   file being created, defaulting to none
 * @param ctime the creation time for the new file, defaulting to the current time
 * @param altid an alternate id value for the new file, defaulting to none
 * @param replace true if an existing file of the same name should be replaced, but
 *                defaulting to false
 */
class CfsCreateOptions(val dataSeqnum : Long, val ctime : Long, val altid : Long, val replace : Boolean) extends CfsOptions

object CfsCreateOptions {
    def Default = CfsCreateOptions()
    def apply(dataSeqnum : Long = 0L, ctime : Long = millis, altid : Long = 0L, replace : Boolean = false) : CfsCreateOptions = {
        new CfsCreateOptions(dataSeqnum, ctime, altid, replace)
    }
}

class CfsOpenOptions() extends CfsOptions

object CfsOpenOptions {
    val Default = CfsOpenOptions()
    def apply() : CfsOpenOptions = new CfsOpenOptions()
}
