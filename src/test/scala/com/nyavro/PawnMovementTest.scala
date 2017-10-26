package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class PawnMovementTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  "Pawn" should {
    "Move one cell forward" in {
      Board(5, 5).put(2,2)(Pawn(North)).map(_.move(Up)) should === (Board(5, 5).put(1,2)(Pawn(North)))
    }
  }
}
