package com.nyavro

import android.net.wifi.p2p.WifiP2pManager.UpnpServiceResponseListener
import rx.lang.scala.Observable



/**
  * Observable converter. Analyzes stream of movement points into stream of gestures (up, leftUp, rightAndUp, etc)
  * @param source Observable of movement points
  * @param rangeAngle half sector's angle (max = Pi/8 for 8-directional fragmentation)
  */
class GestureStream(source:Observable[Point], rangeAngle: Double) {
  def detect(): Observable[Gesture] = {
    val map = Map[Gesture, Double](
      Up -> 0.0,
      Right -> 0.0,
      Down -> 0.0,
      UpRight -> 0.0
    )
    source
      .zip(source.drop(1))
      .scan(Option.empty[Point], map) {
        case ((None, _), (first, second)) => (None, map)
      }
      .take(1)
      .map(_ => Up)
  }
}
