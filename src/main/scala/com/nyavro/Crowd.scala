package com.nyavro

class Crowd(val list:List[Option[Movable]]){
  def move():Crowd = {
    val (moved,_) = list.foldLeft(List.empty[Option[Movable]], 0) {
      case ((acc, free), None) => (None::acc, free+1)
      case ((acc, free), Some(m)) =>
        val move = m.stepsCount().min(free)
        val (before, after) = acc.splitAt(move)
        if(move<m.stepsCount() && after.headOption.fold(false)(_.exists(_.canMerge(m)))) {
          (before ++ (None:: after.head.map(_.merge(m))::after.drop(1)), move)
        }
        else {
          (before ++ (Some(m) :: after), move)
        }
    }
    Crowd(moved.reverse)
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that:Crowd => that.list==list
    case _ => false
  }

  override def hashCode(): Int = list.hashCode()

  override def toString: String = list.toString
}

object Crowd {
  def apply[A <: Movable](list:Option[A]*) = new Crowd(list.toList)
  def apply[A <: Movable](list:List[Option[A]]) = new Crowd(list)
}
