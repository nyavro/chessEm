package com.nyavro

class Crowd(direction:Direction, val list:List[Option[Movable]]){
  def move():Crowd = {
    val (moved,_) = list.foldLeft(List.empty[Option[Movable]], 0) {
      case ((acc, free), None) => (None::acc, free+1)
      case ((acc, free), Some(m)) =>
        val stepsCount = m.stepsCount(direction)
        val move = stepsCount.min(free)
        val (before, after) = acc.splitAt(move)
        if(move<stepsCount && after.headOption.fold(false)(_.exists(_.canMerge(m)))) {
          (before ++ (None:: after.head.map(_.merge(m))::after.drop(1)), move)
        }
        else {
          (before ++ (Some(m) :: after), move)
        }
    }
    Crowd(direction, moved.reverse)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Crowd => that.list==list
    case _ => false
  }

  override def hashCode(): Int = list.hashCode()

  override def toString: String = list.toString
}

object Crowd {
  def apply[A <: Movable](direction:Direction, list:Option[A]*) = new Crowd(direction, list.toList)
  def apply[A <: Movable](direction:Direction, list:List[Option[A]]) = new Crowd(direction, list)
}
