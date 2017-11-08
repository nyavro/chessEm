package com.nyavro

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

class SampleCheckTest extends Properties("test") {
  property("check") = forAll(Gen.alphaStr.map("prefix" + _)) {
    item => item.startsWith("prefix")
  }
}
