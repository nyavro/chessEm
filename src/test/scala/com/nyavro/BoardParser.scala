package com.nyavro

trait BoardParser {

  protected def parseBoardRow(rowString: String):List[Option[Piece]] =
    rowString.toList.map {
      case '-' => None
      case 'K' => Some(King)
      case 'q' => Some(Queen)
      case 'r' => Some(Rook)
      case 'b' => Some(Bishop)
      case 'k' => Some(Knight)
      case '^' => Some(Pawn(Up))
      case 'v' => Some(Pawn(Down))
      case '>' => Some(Pawn(Right))
      case '<' => Some(Pawn(Left))
    }

  protected def parseBoardRows(rows:List[String]) = new Board(rows.map(parseBoardRow))

}
