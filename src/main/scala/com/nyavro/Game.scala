package com.nyavro

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

case object Pawn extends Piece {
  override val rank = 5
}

case class Cell(row:Int, col:Int, piece:Option[Piece])

class Board(freeCells:Set[(Int,Int)], val cells:List[(Int, Int, Piece)]) {
  def put(cell:(Int, Int), piece:Piece):Option[Board] =
    if (freeCells.contains(cell)) {
      Option(new Board(freeCells-cell, (cell._1, cell._2, piece)::cells))
    }  else {
      None
    }
}

object Board {
  val MaxSize = 10

  def apply(rows:Int, cols:Int): Unit = {
    require(rows>0 && rows <= MaxSize)
    require(cols>0 && cols <= MaxSize)
    new Board((0 until rows).flatMap(row => (0 until cols).map(col => (row, col))).toSet, Nil)
  }
}

case class GameConfig(rows:Int, cols:Int, appearanceMap:Map[Piece, List[Piece]])

class Game(config:GameConfig) {

  def dimensions():(Int,Int) = (config.rows, config.cols)

  def cells():List[Cell] = List()
}
