package com.nyavro

trait Gesture

case object Up extends Gesture
case object UpRight extends Gesture
case object Right extends Gesture
case object RightDown extends Gesture
case object Down extends Gesture
case object DownLeft extends Gesture
case object Left extends Gesture
case object LeftUp extends Gesture

case object RightAndUp extends Gesture
case object RightAndDown extends Gesture
case object DownAndRight extends Gesture
case object DownAndLeft extends Gesture
case object LeftAndUp extends Gesture
case object LeftAndDown extends Gesture
case object UpAndRight extends Gesture
case object UpAndLeft extends Gesture

