package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class CrowdTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  trait ComparableMovable extends Movable {
    override def equals(obj: scala.Any):Boolean = obj match {
      case m: Movable => toString == m.toString
      case _ => false
    }
  }

  "Crowd" should {
    "Do nothing on empty list" in {
      Crowd[Movable](North, None, None, None, None, None).move() should === (Crowd(North, None, None, None, None, None))
      Crowd[Movable](North, None, None, None, None).move() should === (Crowd(North, None, None, None, None))
      Crowd[Movable](North, None, None, None).move() should === (Crowd(North, None, None, None))
      Crowd[Movable](North, None, None).move() should === (Crowd(North, None, None))
      Crowd[Movable](North, None).move() should === (Crowd(North, None))
    }
    "Not move if can't do any moves" in {
      val v = new Movable {def stepsCount(direction:Direction) = 0}
      Crowd(North, None, None, Some(v), None, None).move() should === (Crowd(North, None, None, Some(v), None, None))
      Crowd(North, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None))
      Crowd(North, None, None, Some(v)).move() should === (Crowd(North, None, None, Some(v)))
    }
    "Move one cell forward" in {
      val v = new Movable {def stepsCount(direction:Direction) = 1}
      Crowd(North, None, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None, None))
      Crowd(North, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None))
      Crowd(North, None, None, Some(v)).move() should === (Crowd(North, None, Some(v), None))
    }
    "Move max possible cells forward" in {
      val v = new Movable {def stepsCount(direction:Direction) = 3}
      Crowd(North, None, None, None, None, Some(v), None, None).move() should === (Crowd(North, None, Some(v), None, None, None, None, None))
      Crowd(North, None, None, None, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None, None, None, None))
      Crowd(North, None, None, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None, None, None))
      Crowd(North, None, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None, None))
      Crowd(North, Some(v), None, None).move() should === (Crowd(North, Some(v), None, None))
      Crowd(North, Some(v), None).move() should === (Crowd(North, Some(v), None))
      Crowd(North, Some(v)).move() should === (Crowd(North, Some(v)))
    }
    "Move unlimited movable" in {
      val v = new Movable {def stepsCount(direction:Direction) = Integer.MAX_VALUE}
      Crowd(North, None, None, None, None, None, None, Some(v)).move() should === (Crowd(North, Some(v), None, None, None, None, None, None))
      Crowd(North, None, None, None, None, None, Some(v)).move() should === (Crowd(North, Some(v), None, None, None, None, None))
      Crowd(North, None, None, None, None, Some(v)).move() should === (Crowd(North, Some(v), None, None, None, None))
      Crowd(North, None, None, None, Some(v)).move() should === (Crowd(North, Some(v), None, None, None))
      Crowd(North, None, None, Some(v)).move() should === (Crowd(North, Some(v), None, None))
      Crowd(North, None, Some(v)).move() should === (Crowd(North, Some(v), None))
      Crowd(North, Some(v)).move() should === (Crowd(North, Some(v)))
    }
    "Move several movables" in {
      val one = new Movable {def stepsCount(direction:Direction) = 1}
      val two = new Movable {def stepsCount(direction:Direction) = 2}
      val unlimited = new Movable {def stepsCount(direction:Direction) = Integer.MAX_VALUE}
      Crowd(North, None, None, None, Option(two), None, None, Option(one), None, None, None, None, Option(unlimited)).move() should === (Crowd(North, None, Option(two), None, None, None, Option(one), Option(unlimited), None, None, None, None, None))
      Crowd(North, None, None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, None, None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(North, None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(North, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(North, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(North, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(North, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited), None, None, None, None))
      Crowd(North, Option(one), Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited), None, None, None))
      Crowd(North, Option(one), Option(two), None, None, Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited), None, None))
      Crowd(North, Option(one), Option(two), None, Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited), None))
      Crowd(North, Option(one), Option(two), Option(unlimited)).move() should === (Crowd(North, Option(one), Option(two), Option(unlimited)))
    }
    "Merge movables" in {
      class One extends ComparableMovable {
        def stepsCount(direction:Direction) = 1
        override def canMerge(that: Movable): Boolean = that match {
          case _:One => true
          case _ => false
        }
        override def merge(that: Movable): Movable = that match {
          case _:One => new Two
          case _ => throw new IllegalStateException("Illegal state one")
        }
        override def toString = "one"
      }
      class Two extends ComparableMovable {
        def stepsCount(direction:Direction) = 2
        override def canMerge(that: Movable): Boolean = {
          that match {
            case _: Two => true
            case _ => false
          }
        }
        override def merge(that: Movable): Movable = that match {
          case _:Two => new Four
          case _ => throw new IllegalStateException("Illegal state two")
        }
        override def toString = "two"
      }
      class Four extends ComparableMovable {
        def stepsCount(direction:Direction) = 4
        override def canMerge(that: Movable): Boolean = that match {
          case _:Four => true
          case _ => false
        }
        override def merge(that: Movable):Movable = that match {
          case _:Four => new Unlimited
          case _ => throw new IllegalStateException("Illegal state four")
        }
        override def toString = "one"
      }
      class Unlimited extends ComparableMovable {
        def stepsCount(direction:Direction) = Integer.MAX_VALUE
        override def merge(that: Movable) = throw new IllegalStateException("Illegal state unlimited")
        override def toString = "one"
      }
      val one = new One
      val two = new Two
      val four = new Four
      val unlimited = new Unlimited
      Crowd(North, None, Option(two), Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move().list should === (Crowd(North, Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None).list)
      Crowd(North, None, Option(two), None, Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move().list should === (Crowd(North, Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None).list)
      Crowd(North, Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None).move().list should === (Crowd(North, Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None, None).list)
    }
  }
}
