package com.nyavro

import scala.annotation.tailrec

class TableUtils[A](values:Table[A]) {

  private def replace[T](list:List[T], index:Int, v:T):List[T] = {
    val (pre, post) = list.splitAt(index)
    pre ++ (v::post.drop(1))
  }

  private def flip[K, T, P]: ((K,T) => P) => (T,K) => P = f => (x,y) => f(y,x)

  private def insertColumn[B](col:List[B], table:List[List[B]]):List[List[B]] = col.zip(table).map {case (c, row) => c::row}

  def put(row:Int, col:Int)(value:A):Option[Table[A]] = {
    val (pre, post) = values.splitAt(row)
    post.headOption.map (v => pre ++ (replace(v, col, value)::post.tail))
  }

  def rotate():Table[A] = values.foldLeft(List.fill(values.headOption.fold(0)(_.size))(List.empty[A]))(flip(insertColumn))

  def transpose():Table[A] = values.foldRight(List.fill(values.headOption.fold(0)(_.size))(List.empty[A]))(insertColumn)

  def rotate2():Table[A] = values.reverse.map(_.reverse)

  def rotate3():Table[A] = rotate2().rotate()

  def affects():Table[(Option[A],Option[A],Option[A])] = {
    val filled = List.fill(values.headOption.fold(0)(_.size))(Option.empty[A])
    (filled :: (values.map(_.map(Option.apply)) ++ List(filled))).sliding(3).toList.map {
      case List(prev, cur, next) =>
        (None::None::prev, cur, (next ++ List(None, None)).drop(2)).zipped.map {
          case (a,b,c) => (a,b,c)
        }
    }
  }

  def halves():(Table[A], Table[A]) =
    values.zipWithIndex.foldRight(List.empty[List[A]], List.empty[List[A]]){
      case ((item, index), (even, odd)) => if (index%2==0) (item::even, odd) else (even, item::odd)
    }

  def fromHalves(odds:Table[A]):Table[A] = (values, odds) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x::xs, y::ys) => x::y::xs.fromHalves(ys)
  }

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

  def fromDiagonals(cols:Int):Table[A] = {
    val (start, rest) = values.map(_.reverse).splitAt(cols)
    val (_, b) = (rest ++ List(List())).foldLeft(start, List.empty[List[A]]) {
      case ((buf, res), item) =>
        val (col, rest) = buf.foldLeft(List.empty[A], List.empty[List[A]]) {
          case ((hs, ts), h :: t) => (h :: hs, t :: ts)
          case ((hs, ts), _) => (hs, ts)
        }
        ((item :: rest).reverse.drop(1), col :: res)
    }
    b.reverse.map(_.reverse)
  }

  def fromMinorDiagonals(cols:Int):Table[A] = fromDiagonals(cols).rotate()

  override def toString:String = values.map(_.mkString(",")).mkString(";")
}

