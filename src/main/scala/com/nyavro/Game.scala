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

class Board(val cells:Table[Option[Piece]]) {

  def move(direction: Gesture): Board = {
    direction match {
      case Up => new Board(new Table(cells.rotate().values.map {
        case row if row.last.isEmpty && row.contains(Some(Pawn(North))) => (None::row).init
        case row => row
      }).rotate3())
      case Down => new Board(new Table(cells.rotate3().values.map {
        case row if row.last.isEmpty && row.contains(Some(Pawn(South))) => (None::row).init
        case row => row
      }).rotate())
      case Right => new Board(new Table(cells.values.map {
        case row if row.last.isEmpty && row.contains(Some(Pawn(East))) => (None::row).init
        case row => row
      }))
      case Left => new Board(new Table(cells.rotate2().values.map {
        case row if row.last.isEmpty && row.contains(Some(Pawn(West))) => (None::row).init
        case row => row
      }).rotate2())
      case _ => this
    }
  }

  def put(row:Int, col:Int)(piece:Piece):Option[Board] = cells.put(row, col)(Option(piece)).map(new Board(_))

  override def toString:String = cells.toString
}

object Board {
  val MaxSize = 10

  def apply(rows:Int, cols:Int): Board = {
    require(rows>0 && rows <= MaxSize)
    require(cols>0 && cols <= MaxSize)
    new Board(new Table(List.fill(rows)(List.fill(cols)(Option.empty[Piece]))))
  }
}

case class GameConfig(rows:Int, cols:Int, appearanceMap:Map[Piece, List[Piece]])

class Game(config:GameConfig) {

  def dimensions():(Int,Int) = (config.rows, config.cols)

}
