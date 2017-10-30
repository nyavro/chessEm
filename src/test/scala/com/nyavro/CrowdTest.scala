package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class CrowdTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  trait TestMovable extends Movable {
    override def equals(obj: scala.Any):Boolean = obj match {
      case m: Movable => toString == m.toString
      case _ => false
    }
    override def merge(that: Movable):Movable = this
  }

  "Crowd" should {
    "Do nothing on empty list" in {
      Crowd[Movable](Up, None, None, None, None, None).move() should === (Crowd(Up, None, None, None, None, None))
      Crowd[Movable](Up, None, None, None, None).move() should === (Crowd(Up, None, None, None, None))
      Crowd[Movable](Up, None, None, None).move() should === (Crowd(Up, None, None, None))
      Crowd[Movable](Up, None, None).move() should === (Crowd(Up, None, None))
      Crowd[Movable](Up, None).move() should === (Crowd(Up, None))
    }
    "Not move if can't do any moves" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 0}
      Crowd(Up, None, None, Some(v), None, None).move() should === (Crowd(Up, None, None, Some(v), None, None))
      Crowd(Up, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None))
      Crowd(Up, None, None, Some(v)).move() should === (Crowd(Up, None, None, Some(v)))
    }
    "Move one cell forward" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 1}
      Crowd(Up, None, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None, None))
      Crowd(Up, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None))
      Crowd(Up, None, None, Some(v)).move() should === (Crowd(Up, None, Some(v), None))
    }
    "Move max possible cells forward" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 3}
      Crowd(Up, None, None, None, None, Some(v), None, None).move() should === (Crowd(Up, None, Some(v), None, None, None, None, None))
      Crowd(Up, None, None, None, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None, None, None, None))
      Crowd(Up, None, None, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None, None, None))
      Crowd(Up, None, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None, None))
      Crowd(Up, Some(v), None, None).move() should === (Crowd(Up, Some(v), None, None))
      Crowd(Up, Some(v), None).move() should === (Crowd(Up, Some(v), None))
      Crowd(Up, Some(v)).move() should === (Crowd(Up, Some(v)))
    }
    "Move unlimited movable" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = Integer.MAX_VALUE}
      Crowd(Up, None, None, None, None, None, None, Some(v)).move() should === (Crowd(Up, Some(v), None, None, None, None, None, None))
      Crowd(Up, None, None, None, None, None, Some(v)).move() should === (Crowd(Up, Some(v), None, None, None, None, None))
      Crowd(Up, None, None, None, None, Some(v)).move() should === (Crowd(Up, Some(v), None, None, None, None))
      Crowd(Up, None, None, None, Some(v)).move() should === (Crowd(Up, Some(v), None, None, None))
      Crowd(Up, None, None, Some(v)).move() should === (Crowd(Up, Some(v), None, None))
      Crowd(Up, None, Some(v)).move() should === (Crowd(Up, Some(v), None))
      Crowd(Up, Some(v)).move() should === (Crowd(Up, Some(v)))
    }
    "Move several movables" in {
      val one = new TestMovable {def stepsCount(direction:Gesture) = 1}
      val two = new TestMovable {def stepsCount(direction:Gesture) = 2}
      val unlimited = new TestMovable {def stepsCount(direction:Gesture) = Integer.MAX_VALUE}
      Crowd(Up, None, None, None, Option(two), None, None, Option(one), None, None, None, None, Option(unlimited)).move() should === (Crowd(Up, None, Option(two), None, None, None, Option(one), Option(unlimited), None, None, None, None, None))
      Crowd(Up, None, None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, None, None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Up, None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Up, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Up, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Up, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Up, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited), None, None, None, None))
      Crowd(Up, Option(one), Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited), None, None, None))
      Crowd(Up, Option(one), Option(two), None, None, Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited), None, None))
      Crowd(Up, Option(one), Option(two), None, Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited), None))
      Crowd(Up, Option(one), Option(two), Option(unlimited)).move() should === (Crowd(Up, Option(one), Option(two), Option(unlimited)))
    }
    "Merge movables" in {
      class One extends TestMovable {
        def stepsCount(direction:Gesture) = 1
        override def canMerge(that: Movable): Boolean = that match {
          case _:One => true
          case _ => false
        }
        override def merge(that: Movable):Movable = that match {
          case _:One => new Two
          case _ => throw new IllegalStateException("Illegal state one")
        }
        override def toString = "one"
      }
      class Two extends TestMovable {
        def stepsCount(direction:Gesture) = 2
        override def canMerge(that: Movable): Boolean = {
          that match {
            case _: Two => true
            case _ => false
          }
        }
        override def merge(that: Movable):Movable = that match {
          case _:Two => new Four
          case _ => throw new IllegalStateException("Illegal state two")
        }
        override def toString = "two"
      }
      class Four extends TestMovable {
        def stepsCount(direction:Gesture) = 4
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
      class Unlimited extends TestMovable {
        def stepsCount(direction:Gesture) = Integer.MAX_VALUE
        override def merge(that: Movable):Movable = throw new IllegalStateException("Illegal state unlimited")
        override def toString = "one"
      }
      val one = new One
      val two = new Two
      val four = new Four
      val unlimited = new Unlimited
      Crowd(Up, None, Option(two), Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move().list should === (Crowd(Up, Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None).list)
      Crowd(Up, None, Option(two), None, Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move().list should === (Crowd(Up, Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None).list)
      Crowd(Up, Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None).move().list should === (Crowd(Up, Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None, None).list)
    }
  }
}
