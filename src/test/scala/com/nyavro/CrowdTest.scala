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

  private def parse[A <: Movable](direction:Gesture, str:String,
                                  a:Option[A]=None, b:Option[A]=None, c:Option[A]=None, d:Option[A]=None) =
    Crowd[A](
      direction,
      str.toList.map {
        case '-' => None
        case 'a' => a
        case 'b' => b
        case 'c' => c
        case 'd' => d
        case _ => None
      }
    )

  "Crowd" should {
    "Do nothing on empty list" in {
      parse(Up, "-----").move() should === (parse(Up, "-----"))
      parse(Up, "----").move() should === (parse(Up, "----"))
      parse(Up, "---").move() should === (parse(Up, "---"))
      parse(Up, "--").move() should === (parse(Up, "--"))
      parse(Up, "-").move() should === (parse(Up, "-"))
      parse(Up, "").move() should === (parse(Up, ""))
    }
    "not move if can't do any moves" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 0}
      parse(Up, "--a--", Some(v)).move() should === (parse(Up, "--a--", Some(v)))
      parse(Up, "a--", Some(v)).move() should === (parse(Up, "a--", Some(v)))
      parse(Up, "a--", Some(v)).move() should === (parse(Up, "a--", Some(v)))
      parse(Up, "--a", Some(v)).move() should === (parse(Up, "--a", Some(v)))
    }
    "Move one cell forward" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 1}
      parse(Up, "--a--", Some(v)).move() should === (parse(Up, "-a---", Some(v)))
      parse(Up, "a--", Some(v)).move() should === (parse(Up, "a--", Some(v)))
      parse(Up, "--a", Some(v)).move() should === (parse(Up, "-a-", Some(v)))
    }
    "Move max possible cells forward" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = 3}
      parse(Up, "----a--", Some(v)).move() should === (parse(Up, "-a-----", Some(v)))
      parse(Up, "---a--", Some(v)).move() should === (parse(Up, "a-----", Some(v)))
      parse(Up, "--a--", Some(v)).move() should === (parse(Up, "a----", Some(v)))
      parse(Up, "-a--", Some(v)).move() should === (parse(Up, "a---", Some(v)))
      parse(Up, "a--", Some(v)).move() should === (parse(Up, "a--", Some(v)))
      parse(Up, "a-", Some(v)).move() should === (parse(Up, "a-", Some(v)))
      parse(Up, "a", Some(v)).move() should === (parse(Up, "a", Some(v)))
    }
    "Move unlimited movable" in {
      val v = new TestMovable {def stepsCount(direction:Gesture) = Integer.MAX_VALUE}
      parse(Up, "------a", Some(v)).move() should === (parse(Up, "a------", Some(v)))
      parse(Up, "-----a", Some(v)).move() should === (parse(Up, "a-----", Some(v)))
      parse(Up, "----a", Some(v)).move() should === (parse(Up, "a----", Some(v)))
      parse(Up, "---a", Some(v)).move() should === (parse(Up, "a---", Some(v)))
      parse(Up, "--a", Some(v)).move() should === (parse(Up, "a--", Some(v)))
      parse(Up, "-a", Some(v)).move() should === (parse(Up, "a-", Some(v)))
      parse(Up, "a", Some(v)).move() should === (parse(Up, "a", Some(v)))
    }
    "Move several movables" in {
      val o = Some(new TestMovable {def stepsCount(direction:Gesture) = 1})
      val t = Some(new TestMovable {def stepsCount(direction:Gesture) = 2})
      val u = Some(new TestMovable {def stepsCount(direction:Gesture) = Integer.MAX_VALUE})
      parse(Up, "---b--a----c", o, t, u).move() should === (parse(Up, "-b---ac-----", o, t, u))
      parse(Up, "-----a-b---c", o, t, u).move() should === (parse(Up, "----abc-----", o, t, u))
      parse(Up, "----a-b---c", o, t, u).move() should === (parse(Up, "---abc-----", o, t, u))
      parse(Up, "---a-b---c", o, t, u).move() should === (parse(Up, "--abc-----", o, t, u))
      parse(Up, "--a-b---c", o, t, u).move() should === (parse(Up, "-abc-----", o, t, u))
      parse(Up, "-a-b---c", o, t, u).move() should === (parse(Up, "abc-----", o, t, u))
      parse(Up, "a-b---c", o, t, u).move() should === (parse(Up, "abc----", o, t, u))
      parse(Up, "ab---c", o, t, u).move() should === (parse(Up, "abc---", o, t, u))
      parse(Up, "ab--c", o, t, u).move() should === (parse(Up, "abc--", o, t, u))
      parse(Up, "ab-c", o, t, u).move() should === (parse(Up, "abc-", o, t, u))
      parse(Up, "abc", o, t, u).move() should === (parse(Up, "abc", o, t, u))
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
      val o = Some(new One)
      val t = Some(new Two)
      val f = Some(new Four)
      val u = Some(new Unlimited)
      parse(Up, "-bb-a----d", o, t, f, u).move() should === (parse(Up, "c--ad-----", o, t, f, u))
      parse(Up, "-b-b-a----d", o, t, f, u).move() should === (parse(Up, "bb--ad-----", o, t, f, u))
      parse(Up, "bb--ad-----", o, t, f, u).move() should === (parse(Up, "c--ad------", o, t, f, u))
    }
    "Merge movables 2" in {
      class One extends TestMovable {
        def stepsCount(direction:Gesture) = Integer.MAX_VALUE
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
        override def canMerge(that: Movable): Boolean = false
        override def merge(that: Movable):Movable = throw new IllegalStateException("Illegal state two")
        override def toString = "two"
      }
      class Three extends TestMovable {
        def stepsCount(direction:Gesture) = 0
        override def canMerge(that: Movable) = false
        override def merge(that: Movable):Movable = throw new IllegalStateException("Illegal state two")

        override def toString = "three"
      }
      val o = Some(new One)
      val t = Some(new Two)
      val i = Some(new Three)
      parse(Up, "caaacaa-", o, t, i).move() should === (parse(Up, "cba-cb--", o, t, i))
    }
  }
}
