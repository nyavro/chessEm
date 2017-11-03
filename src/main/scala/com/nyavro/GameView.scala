package com.nyavro

import android.content.Context
import android.graphics._

class GameView(dimensions: Dimensions, extents: Extents)(implicit context: Context) {

  private val Gap = 5

  private lazy val cellBitmap: Bitmap = BitmapFactory.decodeResource(context.getResources, R.drawable.cell)
  private lazy val cellRect = new Rect(0, 0, cellBitmap.getWidth, cellBitmap.getHeight)

  private lazy val piecesIcons = Map(
    "pawn" -> BitmapFactory.decodeResource(context.getResources, R.drawable.pawn),
    "knight" -> BitmapFactory.decodeResource(context.getResources, R.drawable.knight),
    "bishop" -> BitmapFactory.decodeResource(context.getResources, R.drawable.bishop),
    "rook" -> BitmapFactory.decodeResource(context.getResources, R.drawable.rook),
    "queen" -> BitmapFactory.decodeResource(context.getResources, R.drawable.queen),
    "king" -> BitmapFactory.decodeResource(context.getResources, R.drawable.king)
  )

  private lazy val pawnBitmap: Bitmap = BitmapFactory.decodeResource(context.getResources, R.drawable.rook)
  private lazy val pawnRect = new Rect(0, 0, cellBitmap.getWidth, cellBitmap.getHeight)
var gm = Option.empty[Game]
//  private lazy val buffer: Bitmap = Bitmap.createBitmap(Math.max(1, getWidth), Math.max(1, getHeight), Bitmap.Config.ARGB_8888)

  def draw(canvas: Canvas): Unit = {
    val cellSize = (extents.height/dimensions.rows).min(extents.width/dimensions.cols)

    for (r <- 0 until dimensions.rows) {
      for (c <- 0 until dimensions.cols) {
        canvas.drawBitmap(
          cellBitmap,
          cellRect,
          new Rect(c*cellSize+Gap, r*cellSize+Gap, (c+1)*cellSize-Gap, (r+1)*cellSize-Gap),
          new Paint
        )
      }
    }

    gm.foreach {
      game => {
        game.board.cells.zipWithIndex.foreach {
          case (row, i) => {
            row.zipWithIndex.foreach {
              case (Some(item), j) =>
                canvas.drawBitmap(
                  pawnBitmap,
                  pawnRect,
                  new Rect(j*cellSize+2*Gap, i*cellSize+2*Gap, (j+1)*cellSize-2*Gap, (i+1)*cellSize-2*Gap),
                  new Paint
                )
              case _ => {}
            }
          }
        }

      }
    }
  }

  def update(game: Game): Unit = {
    gm = Some(game)
  }
}
