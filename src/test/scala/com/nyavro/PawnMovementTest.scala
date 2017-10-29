package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class PawnMovementTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  "North oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(North)).map(_.move(Up)).get
      val exp = Board(5, 5).put(1,2)(Pawn(North)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(0,3)(Pawn(North)).map(_.move(Up)).get
      val exp = Board(5, 5).put(0,3)(Pawn(North)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(North)).map(_.move(Down)).get
      val exp = Board(5, 5).put(1,1)(Pawn(North)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "South oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(South)).map(_.move(Down)).get
      val exp = Board(5, 5).put(3,2)(Pawn(South)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(4,3)(Pawn(South)).map(_.move(Down)).get
      val exp = Board(5, 5).put(4,3)(Pawn(South)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(South)).map(_.move(Up)).get
      val exp = Board(5, 5).put(1,1)(Pawn(South)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "East oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(East)).map(_.move(Right)).get
      val exp = Board(5, 5).put(2,3)(Pawn(East)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(3,4)(Pawn(East)).map(_.move(Right)).get
      val exp = Board(5, 5).put(3,4)(Pawn(East)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(East)).map(_.move(Left)).get
      val exp = Board(5, 5).put(1,1)(Pawn(East)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "West oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(West)).map(_.move(Left)).get
      val exp = Board(5, 5).put(2,1)(Pawn(West)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(3,0)(Pawn(West)).map(_.move(Left)).get
      val exp = Board(5, 5).put(3,0)(Pawn(West)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(West)).map(_.move(Right)).get
      val exp = Board(5, 5).put(1,1)(Pawn(West)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }
}
