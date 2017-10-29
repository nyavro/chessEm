package com.nyavro
trait Direction

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

trait Movable {
  def stepsCount(direction: Direction): Int
  def canMerge(that:Movable) = false
  def merge(that:Movable):Movable = this
}

trait Piece

case object King extends Piece {
  override def toString: String = "K"
}

case object Queen extends Piece {
  override def toString: String = "q"
}

case object Rook extends Piece {
  override def toString: String = "r"
}

case object Bishop extends Piece {
  override def toString: String = "b"
}

case object Knight extends Piece {
  override def toString: String = "k"
}

case class Pawn(direction:Direction) extends Piece {
  override def toString: String =
    direction match {
      case North => "^"
      case South => "v"
      case East => ">"
      case West => "<"
      case _ => " "
    }

}

class Board(val cells:Table[Option[Piece]]) {

  def move(direction: Gesture): Board = {
    direction match {
      case Up => new Board(cells.rotate().map {
        case row if row.nonEmpty && row.last.isEmpty && row.contains(Some(Pawn(North))) => (None::row).init
        case row => row
      }.rotate3())
      case Down => new Board(cells.rotate3().map {
        case row if row.nonEmpty && row.last.isEmpty && row.contains(Some(Pawn(South))) => (None::row).init
        case row => row
      }.rotate())
      case Right => new Board(cells.map {
        case row if row.nonEmpty && row.last.isEmpty && row.contains(Some(Pawn(East))) => (None::row).init
        case row => row
      })
      case Left => new Board(cells.rotate2().map {
        case row if row.nonEmpty && row.last.isEmpty && row.contains(Some(Pawn(West))) => (None::row).init
        case row => row
      }.rotate2())
      case _ => this
    }
  }

  def put(row:Int, col:Int)(piece:Piece):Option[Board] = cells.put(row, col)(Option(piece)).map(new Board(_))

  override def toString:String =
    cells.map {
      row => row.map {
        case Some(p) => p.toString
        case None => "-"
      }.mkString
    }.mkString(":")
}

object Board {
  val MaxSize = 10

  def apply(rows:Int, cols:Int): Board = {
    require(rows>0 && rows <= MaxSize)
    require(cols>0 && cols <= MaxSize)
    new Board(List.fill(rows)(List.fill(cols)(Option.empty[Piece])))
  }
}

case class GameConfig(rows:Int, cols:Int, appearanceMap:Map[Piece, List[Piece]])

class Game(config:GameConfig) {

  def dimensions():(Int,Int) = (config.rows, config.cols)

}
