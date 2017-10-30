import com.nyavro.{Bishop, Board, East, King, Knight, North, Pawn, Piece, Queen, Rook, South, Up, West}

def parseBoardRow(rowString: String):List[Option[Piece]] =
  rowString.toList.map {
    case '-' => None
    case 'K' => Some(King)
    case 'q' => Some(Queen)
    case 'r' => Some(Rook)
    case 'b' => Some(Bishop)
    case 'k' => Some(Knight)
    case '^' => Some(Pawn(North))
    case 'v' => Some(Pawn(South))
    case '>' => Some(Pawn(East))
    case '<' => Some(Pawn(West))
  }

private def parseBoardRows(rows:List[String]) = new Board(rows.map(parseBoardRow))

val pawnsBoard = parseBoardRows(
  List(
    "-v-<>-",
    "-->--v",
    "<-v-^^",
    "-<-^->",
  )
)
pawnsBoard.move(Up)
