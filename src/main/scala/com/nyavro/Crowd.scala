package com.nyavro

class Crowd(direction:Gesture, val list:List[Option[Movable]]){
  def move():Crowd = {
    val (moved,_) = list.foldLeft(List.empty[Option[Movable]], 0) {
      case ((acc, free), None) => (None::acc, free+1)
      case ((acc, free), Some(m)) =>
        val stepsCount = m.stepsCount(direction)
        val move = stepsCount.min(free)
        val (before, after) = acc.splitAt(move)
        if(move<stepsCount && after.headOption.fold(false)(item => item.exists(_.canMerge(m)))) {
          (before ++ (Option.empty[Movable] :: after.head.map(v => v.merge(m)) :: after.drop(1)), move+1)
        }
        else {
          (before ++ (Some(m) :: after), move)
        }
    }
    new Crowd(direction, moved.reverse)
  }

  def calculateMoves():List[Int] = {
    val (moves, _, _) = list.foldLeft(List.empty[Int], 0, Option.empty[Movable]) {
      case ((acc, free, last), None) => (0::acc, free+1, last)
      case ((acc, free, last), Some(m)) =>
        val steps = m.stepsCount(direction)
        val move = steps.min(free)
        if (move < steps && last.exists(_.canMerge(m))) {
          ((move+1)::acc, move+1, None)
        }
        else {
          (move::acc, move, Some(m))
        }
    }
    moves.reverse
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