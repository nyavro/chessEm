package com.nyavro

import scala.io.Source

trait SamplesLoader {

  private val samplesCount = 50

  private def parse(line:String) = {
    val Array(x,y) = line.split(",")
    Option(Point(x.toInt,y.toInt))
  }

  def samples(name:String):List[List[Option[Point]]] = (0 until samplesCount).toList.map {
    i =>
      Source
        .fromInputStream(this.getClass.getClassLoader.getResourceAsStream(s"$name/$i.txt"))
        .getLines()
        .map(parse)
        .toList ++ List(None, None)
  }
}
