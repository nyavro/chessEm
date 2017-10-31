package com.nyavro

class Crowd(direction:Gesture, val list:List[Option[Movable]]){
  def move():Crowd = {
    val (moved,_) = list.foldLeft(List.empty[Option[Movable]], 0) {
      case ((acc, free), None) => (None::acc, free+1)
      case ((acc, free), Some(m)) =>
        val stepsCount = m.stepsCount(direction)
        val move = stepsCount.min(free)
        val (before, after) = acc.splitAt(move)
        val option = after.headOption
        val bool = option.fold(false)(item => item.exists(_.canMerge(m)))
        if(move<stepsCount && bool) {
          val maybeMovable = after.head.map(v => v.merge(m))
          (before ++ (Option.empty[Movable] :: maybeMovable :: after.drop(1)), move+1)
        }
        else {
          (before ++ (Some(m) :: after), move)
        }
    }
    new Crowd(direction, moved.reverse)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Crowd => that.list==list
    case _ => false
  }

  override def hashCode(): Int = list.hashCode()

  override def toString: String = list.toString
}

object Crowd {
  def apply[A <: Movable](direction:Gesture, list:Option[A]*) = new Crowd(direction, list.toList)
  def apply[A <: Movable](direction:Gesture, list:List[Option[A]]) = new Crowd(direction, list)
}
