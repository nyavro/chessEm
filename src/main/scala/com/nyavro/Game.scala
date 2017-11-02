package com.nyavro

trait Movable {
  def stepsCount(direction: Gesture): Int
  def canMerge(that:Movable) = false
  def merge(that:Movable): Movable
}

trait Piece extends Movable

case object King extends Piece {
  override def toString: String = "K"
  override def stepsCount(direction: Gesture) = direction match {
    case d: LinearGesture => 1
    case _ => 0
  }
  override def merge(that: Movable):Movable = ???
}

case object Queen extends Piece {
  override def toString: String = "q"
  override def stepsCount(direction: Gesture) = direction match {
    case d: LinearGesture => Integer.MAX_VALUE
    case _ => 0
  }
  override def merge(that: Movable):Movable = King
  override def canMerge(that:Movable) = that match {
    case Queen => true
    case _ => false
  }
}

case object Rook extends Piece {
  override def toString: String = "r"
  override def canMerge(that:Movable) = that match {
    case Rook => true
    case _ => false
  }
  override def stepsCount(direction: Gesture) = direction match {
    case s: Straight => Integer.MAX_VALUE
    case _ => 0
  }
  override def merge(that: Movable):Movable = Queen
}

case object Bishop extends Piece {
  override def toString: String = "b"
  override def canMerge(that: Movable): Boolean = that match {
    case Bishop => true
    case _ => false
  }
  override def stepsCount(direction: Gesture) = direction match {
    case d: Diagonal => Integer.MAX_VALUE
    case _ => 0
  }
  override def merge(that: Movable):Movable = Rook
}

case object Knight extends Piece {
  override def toString: String = "k"
  override def canMerge(that: Movable): Boolean = that match {
    case Knight => true
    case _ => false
  }
  override def stepsCount(direction: Gesture) = direction match {
    case d: ComplexGesture => 1
    case _ => 0
  }
  override def merge(that: Movable):Movable = Bishop
}

case class Pawn(start:Gesture) extends Piece {
  override def toString: String =
    start match {
      case Up => "^"
      case Down => "v"
      case Right => ">"
      case Left => "<"
      case _ => " "
    }
  override def canMerge(that: Movable) = that match {
    case Pawn(_) => true
    case _ => false
  }
  override def stepsCount(direction: Gesture) = if (direction==start) 1 else 0
  override def merge(that: Movable) = Knight
}

class Board(val cells:Table[Option[Movable]]) {

  def move(direction: Gesture): Board =
    direction match {
      case Up =>
        new Board(
          cells.rotate3().map {
            row => Crowd(Up, row).move().list
          }.rotate()
        )
      case Down =>
        new Board(
          cells.rotate().map {
            row => Crowd(Down, row).move().list
          }.rotate3()
        )
      case Right =>
        new Board(
          cells.rotate2().map {
            row => Crowd(Right, row).move().list
          }.rotate2()
        )
      case Left =>
        new Board(
          cells.map {
            row => Crowd(Left, row).move().list
          }
        )
      case DownLeft =>
        new Board(
          cells.diagonals().map {
            row => Crowd(DownLeft, row).move().list
          }.fromDiagonals(cells.headOption.fold(0)(_.size))
        )
      case RightDown =>
        new Board(
          cells.rotate().diagonals().map {
            row => Crowd(RightDown, row).move().list
          }.fromDiagonals(cells.size).rotate3()
        )
      case UpRight =>
        new Board(
          cells.rotate2().diagonals().map {
            row => Crowd(UpRight, row).move().list
          }.fromDiagonals(cells.headOption.fold(0)(_.size)).rotate2()
        )
      case LeftUp =>
        new Board(
          cells.rotate3().diagonals().map {
            row => Crowd(LeftUp, row).move().list
          }.fromDiagonals(cells.size).rotate()
        )
      case DownAndLeft =>
        new Board(
          {
            val (evens, odds) = cells.halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(RightAndUp, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(RightAndUp, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds)
          }
        )
      case RightAndDown =>
        new Board(
          {
            val (evens, odds) = cells.rotate().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(RightAndDown, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(RightAndDown, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate3()
          }
        )
      case UpAndRight =>
        new Board(
          {
            val (evens, odds) = cells.rotate2().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(UpAndRight, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(UpAndRight, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate2()
          }
        )
      case LeftAndUp =>
        new Board(
          {
            val (evens, odds) = cells.rotate3().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(LeftAndUp, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(LeftAndUp, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate()
          }
        )
      case RightAndUp =>
        new Board(
          {
            val (evens, odds) = cells.transpose.halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(RightAndUp, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(RightAndUp, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).transpose
          }
        )
      case DownAndRight =>
        new Board(
          {
            val (evens, odds) = cells.transpose.rotate().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(DownAndRight, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(DownAndRight, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate3().transpose
          }
        )
      case LeftAndDown =>
        new Board(
          {
            val (evens, odds) = cells.transpose.rotate2().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(LeftAndDown, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(LeftAndDown, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate2().transpose
          }
        )
      case UpAndLeft =>
        new Board(
          {
            val (evens, odds) = cells.transpose.rotate3().halves()
            val movedEvens = evens
              .diagonals()
              .map {
                row => Crowd(UpAndLeft, row).move().list
              }
              .fromDiagonals(evens.headOption.fold(0)(_.size))
            val movedOdds = odds
              .diagonals()
              .map {
                row => Crowd(UpAndLeft, row).move().list
              }
              .fromDiagonals(odds.headOption.fold(0)(_.size))
            movedEvens.fromHalves(movedOdds).rotate().transpose
          }
        )
      case _ => throw new RuntimeException("Not implemented")
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

class Game(config:GameConfig, onGesture: Gesture => Unit) {

  def move(gesture: Gesture) = onGesture(gesture)

  def dimensions():(Int,Int) = (config.rows, config.cols)
}
