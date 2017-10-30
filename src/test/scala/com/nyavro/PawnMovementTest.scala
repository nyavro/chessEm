package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class PawnMovementTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  "Up oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(Up)).map(_.move(Up)).get
      val exp = Board(5, 5).put(1,2)(Pawn(Up)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(0,3)(Pawn(Up)).map(_.move(Up)).get
      val exp = Board(5, 5).put(0,3)(Pawn(Up)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(Up)).map(_.move(Down)).get
      val exp = Board(5, 5).put(1,1)(Pawn(Up)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "Down oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(Down)).map(_.move(Down)).get
      val exp = Board(5, 5).put(3,2)(Pawn(Down)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(4,3)(Pawn(Down)).map(_.move(Down)).get
      val exp = Board(5, 5).put(4,3)(Pawn(Down)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(Down)).map(_.move(Up)).get
      val exp = Board(5, 5).put(1,1)(Pawn(Down)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "Right oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(Right)).map(_.move(Right)).get
      val exp = Board(5, 5).put(2,3)(Pawn(Right)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(3,4)(Pawn(Right)).map(_.move(Right)).get
      val exp = Board(5, 5).put(3,4)(Pawn(Right)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(Right)).map(_.move(Left)).get
      val exp = Board(5, 5).put(1,1)(Pawn(Right)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }

  "Left oriented Pawn" should {
    "Move one cell forward" in {
      val res = Board(5, 5).put(2,2)(Pawn(Left)).map(_.move(Left)).get
      val exp = Board(5, 5).put(2,1)(Pawn(Left)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Stop at the board" in {
      val res = Board(5, 5).put(3,0)(Pawn(Left)).map(_.move(Left)).get
      val exp = Board(5, 5).put(3,0)(Pawn(Left)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
    "Move only forward" in {
      val res = Board(5, 5).put(1,1)(Pawn(Left)).map(_.move(Right)).get
      val exp = Board(5, 5).put(1,1)(Pawn(Left)).get
      res.cells.zip(exp.cells).foreach {
        case (r, e) => r should === (e)
      }
    }
  }
}
