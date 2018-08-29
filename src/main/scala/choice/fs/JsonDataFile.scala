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

import net.liftweb.json.{Serialization, DefaultFormats, Formats}
import net.liftweb.common.{Failure, Box}
import net.liftweb.util.Helpers._

/**
 * File containing JSON-encoded text.
 */
trait JsonDataNode[N <: CfsVnode with AtomicDataNode[N], T <: AnyRef] { self : N =>
    implicit val formats : Formats = DefaultFormats
    implicit val mf : Manifest[T]
    import Cfs.Log

    def getData : Box[T] = {
        val data : Box[String] = getString
        data flatMap { s =>
            tryo(Serialization.read[T] (s))
        }
    }

    def putData(data : T) : Box[self.type] = {
        val content = Serialization.write[T](data)
        Log.info(s"JsonDataNode put [$content]")
        putBytes (content getBytes ())
    }
}

trait JsonDataFile[F <: CfsFile with JsonDataFile[F, N, T], N <: CfsVnode with AtomicDataNode[N] with JsonDataNode[N, T], T <: AnyRef] { self : F =>

    def getJsonVnode : N = getVnode.asInstanceOf[N]

//    /**
//     * Read the raw bytes of this JSON-formatted file. This is restricted to internal
//     * principals.
//     *
//     * @return a boxed array of bytes
//     */
//    def getBytes : Box[Array[Byte]] = {
//        val principal = getPrincipal
//        if ((principal eq SystemPrincipal) || (principal eq BootPrincipal)) getBytesUnchecked
//        else AccessDeniedFailed
//    }
//
    def getData : Box[T]

    def getDataUnchecked : Box[T] = getJsonVnode.getData

    def putData(data : T) : Box[Boolean]

    def putDataUnchecked(data : T) : Box[Boolean] = {
        getJsonVnode putData data map (_ => true)
    }

    def putString : Box[Boolean] = Failure("putString is not supported for a JsonDataFile")
}
