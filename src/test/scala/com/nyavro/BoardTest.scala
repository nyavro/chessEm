package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class BoardTest extends WordSpecLike with Matchers with BeforeAndAfterAll with BoardParser {

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
    "Move Up" in {
      testBoard.move(Up).toString should === (
        parseBoardRows(
          List(
            "-q-vqq-",
            "-------",
            "-b-k---",
            "-v---<^",
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
            "--qv---",
            "--b--r-",
            "---kqr-",
            "-vb--<q",
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
            "---v---",
            "-----qr",
            "-b-k--r",
            "-v---<-",
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
            "---v---",
            "-----r-",
            "---k-r-",
            "-vb--<-",
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
            "-------",
            "-q-v---",
            "-b-k-q-",
            "-----<-",
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
            "---v---",
            "-----r-",
            "q--k-r-",
            "bv---<-",
            "--->--^",
            "b--K--^",
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
            "---v---",
            "qr-----",
            "-b-kr--",
            "-v--<--",
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
            "q--v---",
            "b----r-",
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
    val testBoard2 = parseBoardRows(List(
      "---v---",
      "-q---r-",
      "-b-k-r-",
      "-v---<-",
      "--->--^"
//      "b--K--^",
//      "qqvqqqv",
//      "-------",
//      "-b--b-r",
//      "--kk--b"
    ))
    "Move DownAndLeft" in {
      testBoard2.move(DownAndLeft).toString should === (
        parseBoardRows(
          List(
            "---v---",
            "-q---r-",
            "-b---r-",
            "-v---<-",
            "--k>--^"
//            "b--K--^",
//            "qqvqqqv",
//            "-------",
//            "-b--b-r",
//            "--kk--b"
          )
        ).toString
      )
    }
//    "Move RightAndUp" in {
//      testBoard.move(RightAndUp).toString should === (
//        parseBoardRows(
//          List(
//            "---v---",
//            "-q---r-",
//            "-b-k-r-",
//            "-v---<-",
//            "--->--^",
//            "b--K--^",
//            "qqvqqqv",
//            "-------",
//            "-b--b-r",
//            "--kk--b"
//          )
//        ).toString
//      )
//    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
    //    "Move RightAndUp" in {
    //      testBoard.move(RightAndUp).toString should === (
    //        parseBoardRows(
    //          List(
    //            "---v---",
    //            "-q---r-",
    //            "-b-k-r-",
    //            "-v---<-",
    //            "--->--^",
    //            "b--K--^",
    //            "qqvqqqv",
    //            "-------",
    //            "-b--b-r",
    //            "--kk--b"
    //          )
    //        ).toString
    //      )
    //    }
  }
}
