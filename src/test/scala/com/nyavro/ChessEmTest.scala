package com.nyavro

import org.junit.Assert._
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.robolectric.annotation.Config
import org.robolectric.{Robolectric, RobolectricTestRunner}

@RunWith(classOf[RobolectricTestRunner])
@Config(manifest = "src/main/AndroidManifest.xml")
@Ignore
class ChessEmTest {
  @Test def testButtonPressed(): Unit = {
    val activity = Robolectric.setupActivity(classOf[ChessEm])
    assertTrue(activity.meToo.text == "Me too")
    activity.redBtn.performClick()
    assertTrue(activity.meToo.text == "PRESSED")
  }
}