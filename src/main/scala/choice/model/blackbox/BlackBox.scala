/**
  * Copyright © 2011-2018 The Board of Trustees of The Leland Stanford Junior University.
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
package choice.model.blackbox

import choice.lib.JsonHelpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._

import _root_.scala.util.Random

object Edge extends Enumeration {
    val Top, Bottom, Left, Right = Value
}


object EdgeState extends Enumeration {
    val Unused, Origin, Target, Reflect, Hit = Value
}

abstract class Cell

case class EdgeCell(row : Int, column : Int) extends Cell {
    var state : EdgeState.Value = EdgeState.Unused
    var path : List[Cell] = _
    var other : EdgeCell = this
}

case class BoardCell() extends Cell {
    var hasBall_? = false
    def setBall() : Unit = { hasBall_? = true }
    def setNoBall() : Unit = { hasBall_? = false }
}

object mygame extends SessionVar[Box[BlackBox]](Empty)

object BlackBox {
    implicit val formats : DefaultFormats.type = DefaultFormats
        
    def init() : Unit = {
        LiftRules.dispatch.prepend {
            // register the user operations handler
            case r @ Req(_, _, PostRequest) if r.param("api") == Full("blackbox") ⇒ () ⇒ handleOp(r)
        }
    }
    
    def handleOp(req : Req) : Box[LiftResponse] = {
        req.json.foreach {
            json => {
                val op = (json \ "op").extract[String]
                op match {
                    case "init" ⇒
                        val width = (json \ "width").extract[Int]
                        val height = (json \ "height").extract[Int]
                        val nballs = (json \ "nballs").extract[Int]
                        mygame(Full(new BlackBox(width, height, nballs)))
                        return SimpleResponse(0, "ok")
                    case "fire" ⇒
                        if (mygame.is.isEmpty) {
                            return SimpleResponse(-1, "no game in progress")
                        }
                        val row = (json \ "row").extract[Int]
                        val column = (json \ "column").extract[Int]
                        mygame.is.foreach { bb ⇒
                            val ecell = bb.fire(row, column)
                            val respmap : Map[String, Any] = Map("status" → ecell.state.id,
                            									 "row" → ecell.row,
                            									 "column" → ecell.column)
                            return MapResponse(respmap)
                        }
                    case "show" ⇒
                        if (mygame.is.isEmpty) {
                            return SimpleResponse(-1, "no game in progress")
                        }
                        mygame.is.foreach { bb ⇒
                            val balls = bb.show()
                            val respmap : Map[String, Any] = Map("status" → 0, "balls" → balls)
                            return MapResponse(respmap)
                        }
                    case _ ⇒
                }
            }
        }
    InvalidOperation
    }
}

class BlackBox(width : Int, height : Int, nballs : Int) extends Logger {
    val board : Array[Array[Cell]] = Array.ofDim[Cell](height + 2, width + 2)
    for (row ← 0 until height + 2; col ← 0 until width + 2) {
        if ((row == 0) || (row > height) || (col == 0) || (col > width)) {
            board(row)(col) = EdgeCell(row, col).asInstanceOf[Cell]
        }
        else {
            board(row)(col) = BoardCell().asInstanceOf[Cell]
        }
    }
    
    // Place balls at random locations in the board
    for (_ ← 0 until nballs) {
        var bcell : BoardCell = null
        var cnt = 0
        do {
            val row = Random.nextInt(height) + 1
            val col = Random.nextInt(width) + 1
            bcell = board(row)(col).asInstanceOf[BoardCell]
            cnt += 1
            info("ball at (" + row + ", " + col + ")")
        } while (bcell.hasBall_? && (cnt < 1000))
        bcell.setBall()
    }
    
    def edgeDirection(ecell : EdgeCell) : (Int, Int) = {
        if (ecell.row == 0) (0, 1)
        else if (ecell.row > height) (0, -1)
        else if (ecell.column == 0) (1, 0)
        else (-1, 0)
    }
    
    def nextCell(row : Int, col : Int, xdir : Int, ydir : Int) : Cell = {
        board(row + ydir)(col + xdir)
    }
    
    def plusCell(row : Int, col : Int, xdir : Int, ydir : Int) : Cell = {
        nextCell(row + ydir, col + xdir, ydir, xdir)
    }
    
    def minusCell(row : Int, col : Int, xdir : Int, ydir : Int) : Cell = {
        nextCell(row + ydir, col + xdir, -ydir, -xdir)
    }
    
    def fire(ecell : EdgeCell) : EdgeCell = {
        if (ecell.state == EdgeState.Unused) {
            var done = false
            var row = ecell.row
            var col = ecell.column
            var (xdir, ydir) = edgeDirection(ecell)
            while (!done) {
                nextCell(row, col, xdir, ydir) match {
                    case b @ BoardCell() ⇒
                        if (b.hasBall_?) {
                            ecell.other = ecell
                            ecell.state = EdgeState.Hit
                            done = true
                        }
                        else {
                            var advance = true
                            val pcell = plusCell(row, col, xdir, ydir)
                            val mcell = minusCell(row, col, xdir, ydir)
                            (pcell, mcell) match {
                                case (pc @ BoardCell(), mc @ BoardCell()) ⇒
                                    if (pc.hasBall_? && mc.hasBall_?) {
                                        ecell.other = ecell
                                        ecell.state = EdgeState.Reflect
                                        advance = false
                                        done = true
                                    }
                                    else if (pc.hasBall_?) {
                                        val tmp = -xdir
                                        xdir = -ydir
                                        ydir = tmp
                                        advance = false
                                    }
                                    else if (mc.hasBall_?) {
                                        val tmp = xdir
                                        xdir = ydir
                                        ydir = tmp
                                        advance = false
                                    }
                                case (pc @ BoardCell(), EdgeCell(_, _)) ⇒
                                    if (pc.hasBall_?) {
                                        val tmp = -xdir
                                        xdir = -ydir
                                        ydir = tmp
                                        advance = false
                                    }
                                case (EdgeCell(_, _), mc @ BoardCell()) ⇒
                                    if (mc.hasBall_?) {
                                        val tmp = xdir
                                        xdir = ydir
                                        ydir = tmp
                                        advance = false
                                    }
                                case (_, _) ⇒ // Edge cells on both sides!
                            }
                            if (advance) {
                                row += ydir
                                col += xdir
                            }
                            else if ((row == ecell.row) && (col == ecell.column)) {
                                ecell.state = EdgeState.Reflect
                                ecell.other = ecell
                                done = true
                            }
                        }
                    case e @ EdgeCell(_, _) ⇒
                        ecell.other = e
                        if (e == ecell) {
                            ecell.state = EdgeState.Reflect
                        }
                        else {
                            e.other = ecell
                            e.state = EdgeState.Target
                            ecell.state = EdgeState.Origin
                        }
                        done = true
                }
            }
            
        }
        ecell.other
    }
    
    def fire(row : Int, column : Int) : EdgeCell = {
        board(row)(column) match {
            case e @ EdgeCell(_, _) ⇒ fire(e)
            case BoardCell() ⇒
                error("attempt to fire from a board cell")
                throw new RuntimeException("attempt to fire from a board cell")
        }
    }
    
    def show() : Array[Array[Int]] = {
        val result = new Array[Array[Int]](nballs)
        var k = 0
        for (row ← 1 to height; col ← 1 to width) {
            val cell = board(row)(col)
            cell match {
                case bc @ BoardCell() if bc.hasBall_? ⇒
                    val coord = new Array[Int](2)
                    coord(0) = row
                    coord(1) = col
                    result(k) = coord
                    k += 1
                    if (k >= nballs) {
                        return result
                    }
                case _ ⇒
            }
        }
        result
    }
}

