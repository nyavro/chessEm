package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import rx.lang.scala.Observable

class GestureDetectorTest extends WordSpecLike with Matchers with BeforeAndAfterAll with SamplesLoader {

  "GestureStream" should {
    "translate stream of directions into stream of gestures: up" in {
      samples("up").zipWithIndex.foreach {
        case (sample,index) => (new GestureStream(Observable.from(sample)).detect().toList.toBlocking.single, index) should === (List(Up), index)
      }
    }
    "translate stream of directions into stream of gestures: upRight" in {
      samples("ur").zipWithIndex.foreach {
        case (sample,index) => (new GestureStream(Observable.from(sample)).detect().toList.toBlocking.single, index) should === (List(UpRight), index)
      }
    }
//    "translate stream of directions into stream of gestures: upAndRight" in {
//      samples("u_r").zipWithIndex.foreach {
//        case (sample,index) => (new GestureStream(Observable.from(sample)).detect().toList.toBlocking.single, index) should === (List(UpAndRight), index)
//      }
//    }
  }
}
