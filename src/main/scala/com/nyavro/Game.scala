package com.nyavro

trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

trait Piece {
  def rank: Int
}

case object King extends Piece {
  override val rank = 0
}

case object Queen extends Piece {
  override val rank = 1
}

case object Rook extends Piece {
  override val rank = 2
}

case object Bishop extends Piece {
  override val rank = 3
}

case object Knight extends Piece {
  override val rank = 4
}

case class Pawn(direction:Direction) extends Piece {
  override val rank = 5
}

class Board(val cells:List[List[Option[Piece]]]) {

  private def transpose() = {
    cells
  }

  def replace[A](list:List[A], index:Int, v:A):List[A] = {
    val (pre, post) = list.splitAt(index)
    pre ++ (v::post.drop(1))
  }

  def move(direction: Gesture): Board = {
    direction match {
      case Up => {}
      case _ => {}
    }
    this
  }

  def put(row:Int, col:Int)(piece:Piece):Option[Board] = {
    val (pre, post) = cells.splitAt(row)
    post.headOption.map (v => new Board(pre ++ (replace(v, col, Option(piece))::post.tail)))
  }

  override def toString:String = cells.toString
}

object Board {
  val MaxSize = 10

  def apply(rows:Int, cols:Int): Board = {
    require(rows>0 && rows <= MaxSize)
    require(cols>0 && cols <= MaxSize)
    new Board(
      List.fill(rows)(List.fill(cols)(Option.empty[Piece]))
    )
  }
}

case class GameConfig(rows:Int, cols:Int, appearanceMap:Map[Piece, List[Piece]])

class Game(config:GameConfig) {

  def dimensions():(Int,Int) = (config.rows, config.cols)

}
