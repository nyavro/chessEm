package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class BoardTableMoveTest extends WordSpecLike with Matchers with BeforeAndAfterAll with BoardParser {

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
    val pawnsBoard = parseRows(
      "-v-<>-",
      "-->--v",
      "<-v-^^",
      "-<-^->"
    )
    "Move Left" in {
      pawnsBoard.move(
        parseMovesRows(
          "...1..",
          "......",
          "......",
          ".1...."
        ),
        Left
      ).toString should === (
        parseRows(
          "-v<->-",
          "-->--v",
          "<-v-^^",
          "<--^->"
        ).toString
      )
    }
    "Move Right" in {
      pawnsBoard
        .move(
          parseMovesRows(
            "......",
            "......",
            "...1..",
            ".1...."
          ),
          Right
        ).toString should === (
        parseRows(
          "-v-<->",
          "--->-v",
          "<-v-^^",
          "-<-^->"
        ).toString
      )
    }
    "Move Up" in {
      pawnsBoard
        .move(
          parseMovesRows(
            "..1.",
            "..1.",
            "...1",
            "....",
            "....",
            "...."
          ),
          Up
        ).toString should === (
        parseRows(
          "-v-<>-",
          "-->-^k",
          "<-v^--",
          "-<--->"
        ).toString
      )
    }
    "Move Down" in {
      pawnsBoard
        .move(
          parseMovesRows(
            "....",
            "...1",
            ".1..",
            "....",
            "....",
            "..1."
          ),
          Down
        ).toString should === (
        parseBoardRows(
          List(
            "---<>-",
            "-v>---",
            "<---^k",
            "-<v^->"
          )
        ).toString
      )
    }
  }
  "Bishop only" should {
    val bishopsBoard = parseRows(
      "-----b",
      "---b--",
      "b-----",
      "----b-"
    )
    "Move DownLeft" in {
      bishopsBoard
        .move(
          parseMovesRows(
            ".",
            "..",
            "...",
            "....",
            "..2.",
            "...3",
            "...",
            "..",
            "."
          ),
          DownLeft
        ).toString should === (
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
    val bishopsBoard2 = parseRows(
      "-----b",
      "---b--",
      "b-----",
      "----b-"
    )
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
    "Ignore other directions moves" in {
      bishopsBoard.move(Left).toString should === (bishopsBoard.toString)
      bishopsBoard.move(Right).toString should === (bishopsBoard.toString)
      bishopsBoard.move(Up).toString should === (bishopsBoard.toString)
      bishopsBoard.move(Down).toString should === (bishopsBoard.toString)
    }
  }
  "Rook only" should {
    val rooksBoard = parseBoardRows(
      List(
        "-----r",
        "--r---",
        "-r----",
        "----r-"
      )
    )
    "Move Left" in {
      rooksBoard.move(Left).toString should === (
        parseBoardRows(
          List(
            "r-----",
            "r-----",
            "r-----",
            "r-----"
          )
        ).toString
      )
    }
    "Move Right" in {
      rooksBoard.move(Right).toString should === (
        parseBoardRows(
          List(
            "-----r",
            "-----r",
            "-----r",
            "-----r"
          )
        ).toString
      )
    }
    "Move Up" in {
      rooksBoard.move(Up).toString should === (
        parseBoardRows(
          List(
            "-rr-rr",
            "------",
            "------",
            "------"
          )
        ).toString
      )
    }
    "Move Down" in {
      rooksBoard.move(Down).toString should === (
        parseBoardRows(
          List(
            "------",
            "------",
            "------",
            "-rr-rr"
          )
        ).toString
      )
    }
    "Ignore other directions moves" in {
      rooksBoard.move(LeftUp).toString should === (rooksBoard.toString)
      rooksBoard.move(RightDown).toString should === (rooksBoard.toString)
      rooksBoard.move(UpRight).toString should === (rooksBoard.toString)
      rooksBoard.move(DownLeft).toString should === (rooksBoard.toString)
    }
  }
  "All pieces" should {
    val testBoard = parseBoardRows(List(
      "--kv---",
      "-q---r-",
      "-b-k-r-",
      "-vb--<-",
      "--->--^",
      "b--K--^",
      "qqvqqqv",
      "-------",
      "-b--b-r",
      "--kk--b"
    ))
    "Move Up" in {
      testBoard.move(Up).toString should === (
        parseBoardRows(
          List(
            "-qkvqq-",
            "-------",
            "-b-k---",
            "-vb--<^",
            "-q->-q^",
            "b--K---",
            "q-vq--v",
            "------r",
            "-b--b--",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move UpRight" in {
      testBoard.move(UpRight).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-qb--r-",
            "---kqr-",
            "-vr--<q",
            "--->Kb^",
            "--q--q^",
            "--v--qv",
            "-----b-",
            "------r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move Right" in {
      testBoard.move(Right).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-----qr",
            "-b-k--r",
            "-vb--<-",
            "---->-^",
            "b---K-^",
            "-Kv-qKv",
            "-------",
            "-b--b-r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move RightDown" in {
      testBoard.move(RightDown).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-----r-",
            "---k-r-",
            "-vr--<-",
            "--->--^",
            "-----q^",
            "--v-K-v",
            "-----qq",
            "-bqb-qr",
            "--kkqbb"
          )
        ).toString
      )
    }

    "Move Down" in {
      testBoard.move(Down).toString should === (
        parseBoardRows(
          List(
            "--k----",
            "-q-v---",
            "-b-k-q-",
            "--b--<-",
            "-v->--^",
            "b-----^",
            "---K---",
            "-qv-q-v",
            "-b-qb-r",
            "q-kk-qb"
          )
        ).toString
      )
    }
    "Move DownLeft" in {
      testBoard.move(DownLeft).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-----r-",
            "q--k-r-",
            "bv---<-",
            "--->--^",
            "r--K--^",
            "q-v---v",
            "q------",
            "-q-qb-r",
            "bqkk--b"
          )
        ).toString
      )
    }
    "Move Left" in {
      testBoard.move(Left).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "qr-----",
            "-b-kr--",
            "-vb-<--",
            "--->--^",
            "b-K---^",
            "K-vKq-v",
            "-------",
            "-b--br-",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move LeftUp" in {
      testBoard.move(LeftUp).toString should === (
        parseBoardRows(
          List(
            "q-kv---",
            "r----r-",
            "---k-r-",
            "qv---<-",
            "-bK>--^",
            "b--qq-^",
            "qqv---v",
            "b--b---",
            "------r",
            "--kk---"
          )
        ).toString
      )
    }
    "Move DownAndLeft" in {
      testBoard.move(DownAndLeft).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-q---r-",
            "-b---r-",
            "-vb--<-",
            "--k>--^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "-b--b-r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move RightAndDown" in {
      testBoard.move(RightAndDown).toString should === (
        parseBoardRows(
          List(
            "---v---",
            "-q--kr-",
            "-b-k-r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "-b--b-r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move UpAndRight" in {
      testBoard.move(UpAndRight).toString should === (
        parseBoardRows(
          List(
            "--kvk--",
            "-q---r-",
            "-b---r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "---kk--",
            "-b--b-r",
            "------b"
          )
        ).toString
      )
    }
    "Move LeftAndUp" in {
      testBoard.move(LeftAndUp).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-q---r-",
            "-b-k-r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "kb--b-r",
            "---k--b"
          )
        ).toString
      )
    }
    "Move RightAndUp" in {
      testBoard.move(RightAndUp).toString should === (
        parseBoardRows(
          List(
            "--kv---",
            "-q---r-",
            "-b-k-r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "-b--bkr",
            "--k---b"
          )
        ).toString
      )
    }
    "Move DownAndRight" in {
      testBoard.move(DownAndRight).toString should === (
        parseBoardRows(
          List(
            "---v---",
            "-q---r-",
            "-b-k-r-",
            "-vb--<-",
            "--->k-^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "-b--b-r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move LeftAndDown" in {
      testBoard.move(LeftAndDown).toString should === (
        parseBoardRows(
          List(
            "---v---",
            "kq---r-",
            "-b-k-r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "-------",
            "-b--b-r",
            "--kk--b"
          )
        ).toString
      )
    }
    "Move UpAndLeft" in {
      testBoard.move(UpAndLeft).toString should === (
        parseBoardRows(
          List(
            "--bv---",
            "-q---r-",
            "-b---r-",
            "-vb--<-",
            "--->--^",
            "b--K--^",
            "qqvqqqv",
            "-kk----",
            "-b--b-r",
            "------b"
          )
        ).toString
      )
    }
  }
}