package com

import scala.language.implicitConversions

package object nyavro {
  type Table[A] = List[List[A]]
  implicit def list2table[A](list: Table[A]):TableUtils[A] = new TableUtils(list)
}
