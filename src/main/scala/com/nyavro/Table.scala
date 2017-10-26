package com.nyavro

class Table[+A](values:List[List[A]]) {

  private def replace[T](list:List[T], index:Int, v:T):List[T] = {
    val (pre, post) = list.splitAt(index)
    pre ++ (v::post.drop(1))
  }

  def put(row:Int, col:Int)(value:A):Option[Table[A]] = {
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
}
