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
package choice.lib

import net.liftweb.common.{Empty, EmptyBox, Box}

/**
 * Monkey patch for Box[T].
 *
 * @param box a Box[T] that is being extended
 * @tparam T the type contained by the Box
 */
class ExtendedBox[T](val box : Box[T]) extends AnyVal {
    /**
     * Execute a specified function if a box is not Full. Return the original
     * box regardless.
     *
     * @param f a function with no arguments and presumably side-effects, to
     *          be executed when the box is not Full
     * @return the original box, Full or not
     */
    def ifnot(f : () ⇒ Unit) : Box[T] = {
        box match {
            case _ : EmptyBox ⇒ f()
            case _ ⇒
        }
        box
    }

    /**
     * Execute a specified function if a box is Empty. The function should return
     * a box of the same type.
     *
     * @param f an expression which produces a Box[T], evaluated only if
     *          this box is Empty
     * @return the original box if it was not Empty, otherwise the result of f
     */
    def ifempty(f : ⇒ Box[T]) : Box[T] = {
        box match {
            case Empty ⇒ f
            case _ ⇒ box
        }
    }

    /**
     * This does what I hoped Box pass() would do - execute a function on the value
     * in a Full box, but always return the original box.
     *
     * @param f a function to operate on the value of a Full box, presumably with
     *          side-effects
     * @return the original box, Full or not
     */
    def use(f : T ⇒ Unit) : Box[T] = {
        box foreach f
        box
    }
}

object ExtendedBox {
    import scala.language.implicitConversions
    implicit def extendBox[T](box : Box[T]): ExtendedBox[T] = new ExtendedBox(box)
}