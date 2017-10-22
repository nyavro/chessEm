package com.nyavro

trait Gesture

trait LinearGesture extends Gesture

case object Up extends LinearGesture
case object UpRight extends LinearGesture
case object Right extends LinearGesture
case object RightDown extends LinearGesture
case object Down extends LinearGesture
case object DownLeft extends LinearGesture
case object Left extends LinearGesture
case object LeftUp extends LinearGesture

case object RightAndUp extends Gesture
case object RightAndDown extends Gesture
case object DownAndRight extends Gesture
case object DownAndLeft extends Gesture
case object LeftAndUp extends Gesture
case object LeftAndDown extends Gesture
case object UpAndRight extends Gesture
case object UpAndLeft extends Gesture

object LinearGesture {

  val SectorHalfAngle = Math.PI/8.0

  def byAngle(angle: Double)(rejectInterval:Double = 0.0):Option[LinearGesture] = {
    val index = ((8.0*angle + Math.PI) / (2.0*Math.PI)).floor.toInt % 8
    val res = Array(Right, UpRight, Up, LeftUp, Left, DownLeft, Down, RightDown).apply(index)
    if (rejectInterval > 0.0) {
      val minAngle = SectorHalfAngle*(index*2.0 - 1.0)
      val maxAngle = minAngle + 2*SectorHalfAngle
      val restrictions =
        if (index > 0) List((minAngle+rejectInterval, maxAngle-rejectInterval))
        else List((0, SectorHalfAngle-rejectInterval), (2.0*Math.PI - SectorHalfAngle + rejectInterval, 2.0*Math.PI))
      if (restrictions.exists{case (min, max) => angle <= max && angle > min}) Option(res)
      else None
    }
    else Option(res)
  }
}
