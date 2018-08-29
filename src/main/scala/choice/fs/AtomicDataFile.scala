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

import net.liftweb.common.{Failure, Box}
import choice.actor.DbManager
import net.liftweb.util.Helpers._

/**
 * This trait is used by CfsVnodes that support atomic access to the data of
 * the associated file. That is, the data can be read or written in one
 * operation.
 */
trait AtomicDataNode[N <: CfsVnode] { self : N ⇒

    /**
     * Read all the file data and return it as an array of bytes.
     *
     * @return a boxed array of bytes if successful, or Failure on error.
     *         Empty should not be returned. An empty file is indicated
     *         by an empty array.
     */
    def getBytes : Box[Array[Byte]] = getResource.getBytes

    def getString : Box[String] = getBytes map (new String(_))

    /**
     * Write all the file data from an array of bytes.
     *
     * @param data the new file data
     * @return this Vnode, boxed, if successful. Failure on error.
     *         Empty should not be returned.
     */
    def putBytes(data : Array[Byte]) : Box[self.type] = {
        val (dataSeqnum, _) = ChoiceData makeDataFile data
        if (dataSeqnum < 0) Failure(s"failed to create ChoiceData file for resource id $getResourceId")
        else {
            this withWriteLock { () ⇒
                DbManager replaceData (this, dataSeqnum, millis) map (_ ⇒ this)
            }
        }
    }
}

trait AtomicDataFile[F <: CfsFile with AtomicDataFile[F, N], N <: CfsVnode with AtomicDataNode[N]] { self : F ⇒

    def getAtomicVnode : N = getVnode.asInstanceOf[N]

    /**
     * Read all the file data and return it as an array of bytes. The principal
     * associated with the file must have appropriate access rights.
     *
     * @return a boxed array of bytes if successful, or Failure on error.
     *         Empty should not be returned. An empty file is indicated
     *         by an empty array.
     */
    def getBytes : Box[Array[Byte]]

    /**
     * Read all file data with no access control check. Normally this should
     * only be called by some other operation that includes an access control
     * check.
     *
     * @return a boxed array of bytes (see getBytes)
     */
    def getBytesUnchecked : Box[Array[Byte]] = getAtomicVnode.getBytes

    /**
     * Read all the file data and return it as a string.
     *
     * @return a boxed string if successful, or Failure on error.
     *         Neither Empty nor null should not be returned. An empty
     *         file is indicated by an empty string.
     */
    def getString : Box[String] = getBytes map (new String (_))

    def getStringUnchecked : Box[String] = getAtomicVnode.getString

    def putBytes(data : Array[Byte]) : Box[Boolean]

    def putBytesUnchecked(data : Array[Byte]) : Box[Boolean] = getAtomicVnode putBytes data map (_ ⇒ true)

    def putString(data : String) : Box[Boolean] = putBytes (data getBytes ())
}
