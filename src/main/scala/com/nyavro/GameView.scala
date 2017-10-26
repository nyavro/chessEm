package com.nyavro

import android.content.Context
import android.graphics._
import android.util.Log
import android.view.{MotionEvent, View}
import org.scaloid.common.TraitView
import rx.lang.scala.subjects.PublishSubject


class GameView(game:Game)(implicit context:Context) extends View(context) with TraitView[GameView] {

  override def basis: GameView = this

  private lazy val cellBitmap: Bitmap = BitmapFactory.decodeResource(getResources, R.drawable.cell)
  private lazy val cellRect = new Rect(0, 0, cellBitmap.getWidth, cellBitmap.getHeight)

  private lazy val pawnBitmap: Bitmap = BitmapFactory.decodeResource(getResources, R.drawable.circle)
  private lazy val pawnRect = new Rect(0, 0, cellBitmap.getWidth, cellBitmap.getHeight)

  private val sub = PublishSubject[Option[Point]]()

  val Gap = 5

  override def onDraw(canvas:Canvas): Unit = {
    super.onDraw(canvas)
    val (rows, cols) = game.dimensions()
    val (screenWidth, screenHeight) = (canvas.getWidth, canvas.getHeight)
    val cellSize = (screenHeight/rows).min(screenWidth/cols)
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        canvas.drawBitmap(
          cellBitmap,
          cellRect,
          new Rect(c*cellSize+Gap, r*cellSize+Gap, (c+1)*cellSize-Gap, (r+1)*cellSize-Gap),
          new Paint
        )
      }
    }
    val pr = 3
    val pc = 5
    canvas.drawBitmap(
      pawnBitmap,
      pawnRect,
      new Rect(pc*cellSize+2*Gap, pr*cellSize+2*Gap, (pc+1)*cellSize-2*Gap, (pr+1)*cellSize-2*Gap),
      new Paint
    )
  }

  override def onTouchEvent(me: MotionEvent):Boolean = {
    if (me.getAction == MotionEvent.ACTION_DOWN || me.getAction == MotionEvent.ACTION_MOVE)
      sub.onNext(Option(Point(me.getX, me.getY())))
    else if (me.getAction == MotionEvent.ACTION_UP)
      sub.onNext(None)
    true
  }

  val Threshold = 100
//
//  def gestures():Observable[Gesture] = {
//    Option.empty
//    sub
//      .zip(sub.drop(1))
//      .scan (Option.empty[Gesture]) {
//        case (acc, item) => None
//      }
//      .flatten
//  }

  override def onScrollChanged(l: Int, t: Int, oldl: Int, oldt: Int): Unit = {
    super.onScrollChanged(l, t, oldl, oldt)
    Log.d("MyAppTag", s"$l $t $oldl $oldt")
  }
}
