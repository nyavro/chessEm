package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import rx.lang.scala.Observable

class GestureDetectorTest extends WordSpecLike with Matchers with BeforeAndAfterAll with SamplesLoader {

  "GestureStream" should {
    "translate stream of directions into stream of gestures: up" in {
      samples("up").foreach {
        sample => new GestureStream(Observable.from(sample), Math.PI/16.0).detect().toList.toBlocking.single should === (List(Up))
      }
    }
    "translate stream of directions into stream of gestures: upRight" in {
      samples("ur").foreach {
        sample => new GestureStream(Observable.from(sample), Math.PI/16.0).detect().toList.toBlocking.single should === (List(UpAndRight))
      }
    }
  }
}
