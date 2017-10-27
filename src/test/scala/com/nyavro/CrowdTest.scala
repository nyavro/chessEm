package com.nyavro

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class CrowdTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  "Crowd" should {
    "Do nothing on empty list" in {
      Crowd[Movable](None, None, None, None, None).move() should === (Crowd(None, None, None, None, None))
      Crowd[Movable](None, None, None, None).move() should === (Crowd(None, None, None, None))
      Crowd[Movable](None, None, None).move() should === (Crowd(None, None, None))
      Crowd[Movable](None, None).move() should === (Crowd(None, None))
      Crowd[Movable](None).move() should === (Crowd(None))
    }
    "Not move if can't do any moves" in {
      val v = new Movable {val stepsCount = 0}
      Crowd(None, None, Some(v), None, None).move() should === (Crowd(None, None, Some(v), None, None))
      Crowd(Some(v), None, None).move() should === (Crowd(Some(v), None, None))
      Crowd(None, None, Some(v)).move() should === (Crowd(None, None, Some(v)))
    }
    "Move one cell forward" in {
      val v = new Movable {val stepsCount = 1}
      Crowd(None, Some(v), None, None).move() should === (Crowd(Some(v), None, None, None))
      Crowd(Some(v), None, None).move() should === (Crowd(Some(v), None, None))
      Crowd(None, None, Some(v)).move() should === (Crowd(None, Some(v), None))
    }
    "Move max possible cells forward" in {
      val v = new Movable {val stepsCount = 3}
      Crowd(None, None, None, None, Some(v), None, None).move() should === (Crowd(None, Some(v), None, None, None, None, None))
      Crowd(None, None, None, Some(v), None, None).move() should === (Crowd(Some(v), None, None, None, None, None))
      Crowd(None, None, Some(v), None, None).move() should === (Crowd(Some(v), None, None, None, None))
      Crowd(None, Some(v), None, None).move() should === (Crowd(Some(v), None, None, None))
      Crowd(Some(v), None, None).move() should === (Crowd(Some(v), None, None))
      Crowd(Some(v), None).move() should === (Crowd(Some(v), None))
      Crowd(Some(v)).move() should === (Crowd(Some(v)))
    }
    "Move unlimited movable" in {
      val v = new Movable {val stepsCount:Int = Integer.MAX_VALUE}
      Crowd(None, None, None, None, None, None, Some(v)).move() should === (Crowd(Some(v), None, None, None, None, None, None))
      Crowd(None, None, None, None, None, Some(v)).move() should === (Crowd(Some(v), None, None, None, None, None))
      Crowd(None, None, None, None, Some(v)).move() should === (Crowd(Some(v), None, None, None, None))
      Crowd(None, None, None, Some(v)).move() should === (Crowd(Some(v), None, None, None))
      Crowd(None, None, Some(v)).move() should === (Crowd(Some(v), None, None))
      Crowd(None, Some(v)).move() should === (Crowd(Some(v), None))
      Crowd(Some(v)).move() should === (Crowd(Some(v)))
    }
    "Move several movables" in {
      val one = new Movable {val stepsCount = 1}
      val two = new Movable {val stepsCount = 2}
      val unlimited = new Movable {val stepsCount:Int = Integer.MAX_VALUE}
      Crowd(None, None, None, Option(two), None, None, Option(one), None, None, None, None, Option(unlimited)).move() should === (Crowd(None, Option(two), None, None, None, Option(one), Option(unlimited), None, None, None, None, None))
      Crowd(None, None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(None, None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(None, None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(None, None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(None, None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(None, None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(None, None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(None, Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(None, Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited), None, None, None, None, None))
      Crowd(Option(one), None, Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited), None, None, None, None))
      Crowd(Option(one), Option(two), None, None, None, Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited), None, None, None))
      Crowd(Option(one), Option(two), None, None, Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited), None, None))
      Crowd(Option(one), Option(two), None, Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited), None))
      Crowd(Option(one), Option(two), Option(unlimited)).move() should === (Crowd(Option(one), Option(two), Option(unlimited)))
    }
    "Merge movables" in {
      class One extends Movable {
        val stepsCount = 1
        override def canMerge(that: Movable): Boolean = that match {
          case _:One => true
          case _ => false
        }
        override def merge(that: Movable): Movable = that match {
          case _:One => new Two
          case _ => throw new IllegalStateException("Illegal state one")
        }
      }
      class Two extends Movable {
        val stepsCount = 2
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
      }
      class Four extends Movable {
        val stepsCount = 4
        override def canMerge(that: Movable): Boolean = that match {
          case _:Four => true
          case _ => false
        }
        override def merge(that: Movable):Movable = that match {
          case _:Four => new Unlimited
          case _ => throw new IllegalStateException("Illegal state four")
        }
      }
      class Unlimited extends Movable {
        val stepsCount:Int = Integer.MAX_VALUE
        override def merge(that: Movable) = throw new IllegalStateException("Illegal state unlimited")
      }
      val one = new One
      val two = new Two
      val four = new Four
      val unlimited = new Unlimited
      Crowd(None, Option(two), Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move() should === (Crowd(Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None))
      Crowd(None, Option(two), None, Option(two), None, Option(one), None, None, None, None, Option(unlimited)).move() should === (Crowd(Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None))
      Crowd(Option(two), Option(two), None, None, Option(one), Option(unlimited), None, None, None, None, None).move() should === (Crowd(Option(four), None, None, Option(one), Option(unlimited), None, None, None, None, None, None))
    }
  }
}
