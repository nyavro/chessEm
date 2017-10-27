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

  override def toString:String = values.map(_.mkString(",")).mkString(";")
}
