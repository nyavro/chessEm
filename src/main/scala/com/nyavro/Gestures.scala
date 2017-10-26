package com.nyavro

trait Gesture
trait LinearGesture extends Gesture
trait ComplexGesture extends Gesture

trait SimpleGesture extends LinearGesture
trait PerspectiveGesture extends LinearGesture

trait RightArea
trait LeftArea
trait UpArea
trait DownArea

case object Up extends PerspectiveGesture with UpArea
case object Right extends PerspectiveGesture with RightArea
case object Down extends PerspectiveGesture with DownArea
case object Left extends PerspectiveGesture with LeftArea

case object UpRight extends SimpleGesture with RightArea with UpArea
case object RightDown extends SimpleGesture with RightArea with DownArea
case object DownLeft extends SimpleGesture with LeftArea with DownArea
case object LeftUp extends SimpleGesture with LeftArea with UpArea

case object RightAndUp extends ComplexGesture
case object RightAndDown extends ComplexGesture
case object DownAndRight extends ComplexGesture
case object DownAndLeft extends ComplexGesture
case object LeftAndUp extends ComplexGesture
case object LeftAndDown extends ComplexGesture
case object UpAndRight extends ComplexGesture
case object UpAndLeft extends ComplexGesture