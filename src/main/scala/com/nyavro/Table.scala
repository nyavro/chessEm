package com.nyavro

class Table[+A](val values:List[List[A]]) {

  private def replace[T](list:List[T], index:Int, v:T):List[T] = {
    val (pre, post) = list.splitAt(index)
    pre ++ (v::post.drop(1))
  }

  def put[B >: A](row:Int, col:Int)(value:B):Option[Table[B]] = {
    val (pre, post) = values.splitAt(row)
    post.headOption.map (v => new Table(pre ++ (replace(v, col, value)::post.tail)))
  }

  def rotate():Table[A] =
    new Table(
      values
        .foldLeft(List.fill(values.head.size)(List.empty[A])) {
          case (acc, row) => row.zip(acc).map {case (res, item) => res::item}
        }
    )

  def rotate2():Table[A] = new Table(values.reverse.map(_.reverse))

  def rotate3():Table[A] = rotate2().rotate()

  def diagonals():List[List[A]] = {
    val (a,b) = values.foldLeft(List.empty[List[A]], List.fill(values.headOption.fold(0)(_.size))(List.empty[A])) {
      case ((res, acc), row) =>
        row.zip(acc).map {case (r, t) => r::t} match {
          case (head::tail) => (head::res, tail ++ List(List.empty[A]))
          case _ => (res, List(List.empty[A]))
        }
    }
    a.reverse ++ b.take(b.length-1)
  }

  override def toString:String = values.map(_.mkString(",")).mkString(";")
}

object Table {
  def fromDiagonals[A](diagonals:List[List[A]], cols:Int):Table[A] =
    new Table({
      val (start, rest) = diagonals.map(_.reverse).splitAt(cols)
      val (_, b) = (rest ++ List(List())).foldLeft(start, List.empty[List[A]]) {
        case ((buf, res), item) =>
          val (col, rest) = buf.foldLeft(List.empty[A], List.empty[List[A]]) {
            case ((hs, ts), h :: t) => (h :: hs, t :: ts)
            case ((hs, ts), _) => (hs, ts)
          }
          ((item :: rest).reverse.drop(1), col :: res)
      }
      b.reverse.map(_.reverse)
    })

  def fromMinorDiagonals[A](minorDiagonals:List[List[A]], cols:Int):Table[A] = fromDiagonals(minorDiagonals, cols).rotate()
}
