package com.nyavro

trait BoardParser {

  protected val piecesMap:Map[Char, Piece] = Map(
    'K' -> King,
    'q' -> Queen,
    'r' -> Rook,
    'b' -> Bishop,
    'k' -> Knight,
    '^' -> Pawn(Up),
    'v' -> Pawn(Down),
    '>' -> Pawn(Right),
    '<' -> Pawn(Left)
  )

  protected def parseBoardRow(rowString: String):List[Option[Piece]] = rowString.toList.map(piecesMap.get)

  protected def parseBoardRows(rows:List[String]) = new Board(rows.map(parseBoardRow))

  protected def parseRows(rows: String*) = parseBoardRows(rows.toList)

  private def parseMovesRows(movesString: String):List[Int] = movesString.toList.map {
    case '.' => 0
    case v => v.toString.toInt
  }

  protected def parseMovesRows(rows: String*):Table[Int] = rows.toList.map(parseMovesRows)
}
