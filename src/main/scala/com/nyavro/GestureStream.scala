package com.nyavro

import rx.lang.scala.Observable

case class Point(x:Double, y:Double) {

  def angle:Double = {
    val len = Math.sqrt(x*x + y*y)
    val v = if (len > 0.0) x/len else 1
    val acos = Math.acos(v)
    if (y > 0.0)
      acos
    else
      Math.PI * 2.0 - acos
  }

  def -(pt:Point) = Point(x-pt.x, y-pt.y)
}

/**
  * Observable converter. Analyzes stream of movement points into stream of gestures (up, leftUp, rightAndUp, etc)
  * @param source Observable of movement points
  */
class GestureStream(source:Observable[Option[Point]]) {
  def detect() = {
    source
      .scan[DetectionState](Initial()) {
      case (acc, item) => acc.process(item)
    }
      .distinctUntilChanged
      .collect{
        case s:Defined => Some(s.gesture)
        case s:Undefined => None
      }
      .collect {
        case Some(v) => v
      }
//
  }
}


trait DetectionState {

  val T = 6.0
  val TM = 20.0

  protected def score(pt:Point):Map[LinearGesture, Double] = {
    val res = Array(Right, RightDown, Down, DownLeft, Left, LeftUp, Up, UpRight)
    val angle = pt.angle
    val im = (8 + (4.0*angle/ Math.PI).floor.toInt) % 8

    val minAngle = im * Math.PI/4
    val r = if (angle > 4.0 && im==0) 0.0 else 4.0*(angle - minAngle)/Math.PI
    Map(res(im) -> (1.0-r), res((im + 1) % 8) -> r)
  }

  protected def merge[A](a:Map[A, Double], b:Map[A, Double]):Map[A, Double] = {
    a ++ b.keys.map(k => k -> (a.getOrElse(k, 0.0) + b.getOrElse(k, 0.0)))
  }

  protected def resolve(fst: LinearGesture, snd: LinearGesture): Gesture = (fst, snd) match {
    case (Up, s:RightArea) => UpAndRight
    case (Up, s:LeftArea) => UpAndLeft
    case (Right, s:UpArea) => RightAndUp
    case (Right, s:DownArea) => RightAndDown
    case (Down, s:RightArea) => DownAndRight
    case (Down, s:LeftArea) => DownAndLeft
    case (Left, s:UpArea) => LeftAndUp
    case (Left, s:DownArea) => LeftAndDown
    case (f, _) => f
  }

  protected def updateScore(map:Map[LinearGesture, Double], start:Point, prev:Point, cur:Point):Map[LinearGesture, Double] =
    List(score(cur-start), score(cur-prev)).fold(map)(merge)

  def process(entry: Option[Point]):DetectionState
}

case class Initial() extends DetectionState {
  def process(entry: Option[Point]):DetectionState = entry match {
    case Some(point) => Started(point)
    case None => this
  }
}

case class Started(start:Point) extends DetectionState {
  def process(entry: Option[Point]):DetectionState = entry match {
    case Some(point) => Undefined(Map(), start, point)
    case None => Initial()
  }
}

case class Undefined(score:Map[LinearGesture, Double], start:Point, prev:Point) extends DetectionState {
  def process(entry: Option[Point]):DetectionState = entry match {
    case Some(point) =>
      val merged = updateScore(score, start, prev, point)
      merged
        .filter{case (_, v) => v > T}
        .toList
        .sortBy{case (_, v) => -v}
        .map(_._1) match {
        case fst::snd::_ => Defined(resolve(fst, snd))
        case fst::_ => PartiallyDefined(Map(), point, point, fst)
        case _ => Undefined(merged, start, point)
      }
    case None => Initial()
  }
}

case class PartiallyDefined(score:Map[LinearGesture, Double], start:Point, prev:Point, gesture: LinearGesture) extends DetectionState {
  def process(entry: Option[Point]):DetectionState = entry match {
    case Some(point) =>
      val merged = updateScore(score, start, prev, point)
      merged
        .filter{case (k, v) => k!=gesture && v > T || v > TM}
        .toList
        .sortBy{case (_, v) => -v}
        .map(_._1)
        .headOption match {
        case Some(fst) => Defined(resolve(gesture, fst))
        case None => PartiallyDefined(merged, start, point, gesture)
      }
    case None => Defined(gesture)
  }
}

case class Defined(gesture: Gesture) extends DetectionState {
  def process(entry: Option[Point]):DetectionState = entry match {
    case Some(point) => this
    case None => Initial()
  }
}