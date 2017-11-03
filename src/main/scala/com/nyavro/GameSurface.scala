package com.nyavro

import android.content.Context
import android.graphics._
import android.util.Log
import android.view.{MotionEvent, View}
import org.scaloid.common.TraitView
import rx.lang.scala.Observable
import rx.lang.scala.subjects.PublishSubject

case class Extents(width:Int, height:Int)

class GameSurface(game:Game)(implicit context:Context) extends View(context) with TraitView[GameSurface] {

  override def basis: GameSurface = this

  private val sub = PublishSubject[Option[Point]]()

  private val view = PublishSubject[GameView]

  private val drw = PublishSubject[Canvas]

  val gameStream:Observable[Game] =
    new GestureStream(sub)
      .detect()
      .scan(game) {
        case (gm, gesture) => gm.move(gesture)
      }

  view.map {
      gv => gameStream.publish.refCount.map {
        gm => (gv, gm)
      }
    }
    .switch
    .subscribe(
      {case (gameView, gm) => gameView.update(gm);invalidate()},
      err => Log.d("GameView gameStream", err.getLocalizedMessage, err),
      () => {}
    )

  view.map {
      gv => drw.publish.refCount.map {
        canvas => (gv, canvas)
      }
    }
    .switch
    .subscribe(
      {case (gameView, canvas) => gameView.draw(canvas)},
      err => Log.d("GameView drw", err.getLocalizedMessage, err),
      () => {}
    )

  override def onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int): Unit = {
    super.onLayout(changed, left, top, right, bottom)
    if(changed) {
      view.onNext(new GameView(game.config.dimensions, Extents(getWidth, getHeight)))
    }
  }

  override def onDraw(canvas:Canvas): Unit = {
    super.onDraw(canvas)
    drw.onNext(canvas)
  }

  override def onTouchEvent(me: MotionEvent):Boolean = {
    if (me.getAction == MotionEvent.ACTION_DOWN || me.getAction == MotionEvent.ACTION_MOVE)
      sub.onNext(Option(Point(me.getX, me.getY())))
    else if (me.getAction == MotionEvent.ACTION_UP) {
      sub.onNext(None)
      sub.onNext(None)
    }
    true
  }
}
