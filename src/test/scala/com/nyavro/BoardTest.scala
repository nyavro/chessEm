package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class BoardTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private def parseBoardRow(rowString: String):List[Option[Piece]] =
    rowString.toList.map {
      case '-' => None
      case 'K' => Some(King)
      case 'q' => Some(Queen)
      case 'r' => Some(Rook)
      case 'b' => Some(Bishop)
      case 'k' => Some(Knight)
      case '^' => Some(Pawn(North))
      case 'v' => Some(Pawn(South))
      case '>' => Some(Pawn(East))
      case '<' => Some(Pawn(West))
    }

  private def parseBoardRows(rows:List[String]) = new Board(rows.map(parseBoardRow))

  "Board" should {
    "Move all possible pieces Left" in {
      parseBoardRows(List("")).move(Left).toString should === (parseBoardRows(List("")).toString)
    }

  }
}
