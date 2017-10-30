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
      case '^' => Some(Pawn(Up))
      case 'v' => Some(Pawn(Down))
      case '>' => Some(Pawn(Right))
      case '<' => Some(Pawn(Left))
    }

  private def parseBoardRows(rows:List[String]) = new Board(rows.map(parseBoardRow))

  private val testBoard = parseBoardRows(List(
    "---v---",
    "-q---r-",
    "-b-k-r-",
    "-v---<-",
    "--->--^",
    "b--K--^",
    "qqvqqqv",
    "-------",
    "-b--b-r",
    "--kk--b"
  ))

  "Border conditions" should {
    "met" in {
      parseBoardRows(List()).move(Left).toString should === (parseBoardRows(List()).toString)
      parseBoardRows(List("")).move(Left).toString should === (parseBoardRows(List("")).toString)
      parseBoardRows(List("-")).move(Left).toString should === (parseBoardRows(List("-")).toString)
      parseBoardRows(List("-", "-")).move(Left).toString should === (parseBoardRows(List("-", "-")).toString)
      parseBoardRows(List("v", ">")).move(Left).toString should === (parseBoardRows(List("v", ">")).toString)
    }
  }

  "Pawns only" should {
    val pawnsBoard = parseBoardRows(
      List(
        "-v-<>-",
        "-->--v",
        "<-v-^^",
        "-<-^->"
      )
    )
    "Move Left" in {
      pawnsBoard.move(Left).toString should === (
        parseBoardRows(
          List(
            "-v<->-",
            "-->--v",
            "<-v-^^",
            "<--^->"
          )
        ).toString
      )
    }
    "Move Right" in {
      pawnsBoard.move(Right).toString should === (
        parseBoardRows(
          List(
            "-v-<->",
            "--->-v",
            "<-v-^^",
            "-<-^->"
          )
        ).toString
      )
    }
    "Move Up" in {
      pawnsBoard.move(Up).toString should === (
        parseBoardRows(
          List(
            "-v-<>-",
            "-->-^v",
            "<-v^-^",
            "-<--->"
          )
        ).toString
      )
    }
    "Move Down" in {
      pawnsBoard.move(Down).toString should === (
        parseBoardRows(
          List(
            "---<>-",
            "-v>--v",
            "<---^^",
            "-<v^->"
          )
        ).toString
      )
    }
  }
  "Bishop only" should {
    val bishopsBoard = parseBoardRows(
      List(
        "-----b",
        "---b--",
        "b-----",
        "----b-"
      )
    )
    "Move DownLeft" in {
      bishopsBoard.move(DownLeft).toString should === (
        parseBoardRows(
          List(
            "------",
            "------",
            "b-----",
            "-bb-b-"
          )
        ).toString
      )
    }
    "Move RightDown" in {
      bishopsBoard.move(RightDown).toString should === (
        parseBoardRows(
          List(
            "-----b",
            "------",
            "------",
            "-b--bb"
          )
        ).toString
      )
    }
    "Move UpRight" in {
      bishopsBoard.move(UpRight).toString should === (
        parseBoardRows(
          List(
            "--b-bb",
            "------",
            "-----b",
            "------"
          )
        ).toString
      )
    }
    "Move LeftUp" in {
      bishopsBoard.move(LeftUp).toString should === (
        parseBoardRows(
          List(
            "-bb--b",
            "------",
            "b-----",
            "------"
          )
        ).toString
      )
    }
  }
}
