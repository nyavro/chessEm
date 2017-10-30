package com.nyavro

trait Gesture

trait LinearGesture extends Gesture

trait ComplexGesture extends Gesture

trait Diagonal extends LinearGesture
trait Straight extends LinearGesture

trait RightArea
trait LeftArea
trait UpArea
trait DownArea


case object Up extends Straight with UpArea
case object Right extends Straight with RightArea
case object Down extends Straight with DownArea
case object Left extends Straight with LeftArea

case object UpRight extends RightArea with UpArea with Diagonal
case object RightDown extends RightArea with DownArea with Diagonal
case object DownLeft extends LeftArea with DownArea with Diagonal
case object LeftUp extends LeftArea with UpArea with Diagonal

case object RightAndUp extends ComplexGesture
case object RightAndDown extends ComplexGesture
case object DownAndRight extends ComplexGesture
case object DownAndLeft extends ComplexGesture
case object LeftAndUp extends ComplexGesture
case object LeftAndDown extends ComplexGesture
case object UpAndRight extends ComplexGesture
case object UpAndLeft extends ComplexGesture