package com.nyavro

import android.graphics.Color
import org.scaloid.common._

class ChessEm extends SActivity {
  lazy val meToo = new STextView("Me too")
  lazy val redBtn = new SButton(R.string.red)
  lazy val game = new Game(GameConfig(6, 6, Map()))
  lazy val gameView = new GameView(game)
  lazy val gestureLayout = new SGestureOverlayView

  override implicit val loggerTag = LoggerTag("MyAppTag")
  onCreate {
    val main = new SVerticalLayout {
      style {
        case b: SButton => b.textColor(Color.RED).onClick(meToo.text = "PRESSED")
        case t: STextView => t textSize 10.dip
        case e: SEditText => e.backgroundColor(Color.YELLOW).textColor(Color.BLACK)
      }
//      STextView("I am 10 dip tall")
      meToo.here
      STextView("I am 15 dip tall") textSize 15.dip // overriding
      new SLinearLayout {
        STextView("Button: ")
        redBtn.here
      }.wrap.here
      //      SEditText("Yellow input field fills the space").fill
      gameView.fill.here
    } padding 20.dip
    contentView(
      main
    )
  }
}