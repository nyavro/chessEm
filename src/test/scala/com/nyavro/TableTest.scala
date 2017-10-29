package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class TableTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  "Table" should {
    "Rotate left" in {
      List[List[Int]]().rotate() should === (List())
    }
    "Return list of diagonals" in {
      List().diagonals() should === (List())
      List(List()).diagonals() should === (List())
      List(
        List(1,2,3,4),
        List(5,6,7,8),
        List(9,8,7,6)
      ).diagonals() should === (
        List(
          List(1),
          List(5,2),
          List(9,6,3),
          List(8,7,4),
          List(7,8),
          List(6)
        )
      )
      List(
        List(1,2,3,4)
      ).diagonals() should === (
        List(
          List(1),
          List(2),
          List(3),
          List(4)
        )
      )
    }
    "can be created from diagonals" in {
      List(List(1)).fromDiagonals(1) should === (List(List(1)))
      List(List(1),List(3,2), List(4)).fromDiagonals(2) should === (List(List(1,2), List(3,4)))
      List(
        List(1),
        List(5,2),
        List(9,6,3),
        List(8,7,4),
        List(7,8),
        List(6)
      ).fromDiagonals(4) should === (
        List(
          List(1,2,3,4),
          List(5,6,7,8),
          List(9,8,7,6)
        )
      )
    }
    "can be created from minor diagonals" in {
      List(List(1)).fromMinorDiagonals(1) should === (List(List(1)))
      List(List(2),List(1,4), List(3)).fromMinorDiagonals(2) should === (List(List(1,2), List(3,4)))
      List(
        List(3),
        List(2,7),
        List(1,6,11),
        List(0,5,10),
        List(4,9),
        List(8)
      ).fromMinorDiagonals(3) should === (
        List(
          List(0,1,2,3),
          List(4,5,6,7),
          List(8,9,10,11)
        )
      )
      List(
        List(5),
        List(3,8),
        List(2,7,12),
        List(1,6,11),
        List(0,5,10),
        List(4,9),
        List(8)
      ).fromMinorDiagonals(3) should === (
        List(
          List(0,1,2,3,5),
          List(4,5,6,7,8),
          List(8,9,10,11,12)
        )
      )
    }
  }
}
