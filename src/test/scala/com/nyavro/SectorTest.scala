package com.nyavro

import org.scalactic.TolerantNumerics
import org.scalatest.{Matchers, WordSpecLike}

class SectorTest extends WordSpecLike with Matchers {
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.00001)
  "It" should {
    "Right sector by angle" in {
      Sector.byAngle()
      Point(10, 0).angle should === (Some(2*Math.PI))
      Point(9.396926207859085f, 3.420201433256684f).angle.get should === (0.3490658503988659)
    }
    "calculate angle of point in sector Up" in {
      Point(0, 10).angle should === (Some(Math.PI/2))
      Point(-3.420201433256684f, 9.396926207859085f).angle.get should === (Math.PI/2 + 0.3490658503988659)
    }
    "calculate angle of point in sector Left" in {
      Point(-10, 0).angle should === (Some(Math.PI))
      Point(-9.396926207859085f, -3.420201433256684f).angle.get should === (Math.PI + 0.3490658503988659)
    }
    "calculate angle of point in sector Down" in {
      Point(0, -10).angle should === (Some(3*Math.PI/2.0))
      Point(3.420201433256684f, -9.396926207859085f).angle.get should === (3*Math.PI/2 + 0.3490658503988659)
    }
    "calculate angle of point in sector RightUp" in {
      Point(4.2261826174069945f, 9.063077870366499f).angle.get should === (1.1344640137963142)
    }
    "calculate angle of point in sector UpLeft" in {
      Point(-9.063077870366499f, 4.2261826174069945f).angle.get should === (Math.PI/2 + 1.1344640137963142)
    }
    "calculate angle of point in sector LeftDown" in {
      Point(-4.2261826174069945f, -9.063077870366499f).angle.get should === (Math.PI + 1.1344640137963142)
    }
    "calculate angle of point in sector DownRight" in {
      Point(9.063077870366499f, -4.2261826174069945f).angle.get should === (3*Math.PI/2 + 1.1344640137963142)
    }
  }
}
