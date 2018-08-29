/**
  * Copyright © 2012-2017 The Board of Trustees of The Leland Stanford Junior University.
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
  *
  * @author Howard Palmer
  *
  */
package choice.model

import net.liftweb.mapper._

class MappedBytes[T <: Mapper[T]](fieldOwner : T, maxLen : Int)
    extends MappedBinary[T](fieldOwner) {

    /**
     * Given the driver type, return the string required to create the column in the database
     */
    override def fieldCreatorString(dbType : DriverType, colName : String) : String =
        s"$colName BINARY($maxLen)${notNullAppender()}"
}
