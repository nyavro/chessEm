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

}
