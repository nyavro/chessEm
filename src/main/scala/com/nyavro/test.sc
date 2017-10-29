val list = List(
  List(1),
  List(5,2),
  List(9,6,3),
  List(8,7,4),
  List(7,8),
  List(6)
)
val (start, rest) = list
  .map(_.reverse).splitAt(4)

def slice(lst:List[List[Int]]) =
  lst.foldLeft(List.empty[Int], List.empty[List[Int]]) {
    case ((hs,ts), h::t) => (h::hs, t::ts)
  }

def slice2(lst:List[List[Int]]) =
  lst.foldRight(List.empty[Int], List.empty[List[Int]]) {
    case (h::t, (hs,ts)) => (h::hs, t::ts)
  }

val u = slice2(List(List(1,2,3), List(4,5), List(7,8,9,10)))

val (w, z) = (rest++List(List())).foldLeft(start,  List.empty[List[Int]]) {
  case ((buf, res), item) =>
    val (col, rest) = slice2(buf)
    ((rest ++ List(item)).drop(1), col::res)
}

val (a, b) = (rest++List(List())).foldLeft(start,  List.empty[List[Int]]) {
  case ((buf, res), item) =>
    val (col, rest) = slice(buf)
    ((item::rest).reverse.drop(1), col::res)
}

def mzip[A](lists:List[List[A]], list:List[A]):List[List[A]] = (lists, list) match {
  case (_, Nil) => lists
  case (x::xs, y::ys) => (y::x)::mzip(xs, ys)
}

val d = list.map(_.reverse).scanLeft(List.fill(3)(List.empty[Int])) {
  case (acc, item) => mzip(acc, item)
}

def merge[A](src:List[A], target:List[List[A]], offset:Int) = {
  def leftMerge(s: List[A], t: List[List[A]]): List[List[A]] = {
    val (before, after) = t.splitAt(t.size.min(s.size))
    before.zip(s).map { case (ys, x) => x :: ys } ++ after
  }
  def rightMerge(s: List[A], t: List[List[A]]): List[List[A]] = {
    val (before, after) = t.splitAt(t.size - s.size)
    before ++ after.zip(s).map { case (ys, x) => x :: ys }
  }
  if (offset < 0) {
    leftMerge(src, target)
  } else {
    rightMerge(src, target)
  }
}

val zz = merge(List(1,2), List(List(3, 4), List(5,6), List(7,8)), 3)

val xx = list.foldLeft(-3, List.fill(4)(List.empty[Int])) {
  case ((off, acc), item) => (off+1, merge(item, acc, off))
}