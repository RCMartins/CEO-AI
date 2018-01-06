package ceo.image

import java.util.StringTokenizer

object Square {
  var INIT_X = 0
  var INIT_Y = 0

  def loadGameArea(data: String): Square = {
    val st = new StringTokenizer(data)
    val left = st.nextToken.toInt
    val top = st.nextToken.toInt
    val right = st.nextToken.toInt
    val bottom = st.nextToken.toInt
    new Square(left, top, right, bottom)
  }
}

class Square() {
  set(0, 0, 0, 0)
  var left = 0
  var top = 0
  var right = 0
  var bottom = 0

  def this(area: Square) {
    this()
    set(area)
  }

  def this(left: Int, top: Int, right: Int, bottom: Int) {
    this()
    set(left, top, right, bottom)
  }

  def set(area: Square): Unit = set(area.left, area.top, area.right, area.bottom)

  def set(left: Int, top: Int, right: Int, bottom: Int): Unit = {
    this.left = left
    this.top = top
    this.right = right
    this.bottom = bottom
    reorder()
  }

  override def toString: String = "[" + left + "," + top + "," + right + "," + bottom + "]"

  def reorder(): Unit = {
    if (left > right) {
      val aux = left
      left = right
      right = aux
    }
    if (top > bottom) {
      val aux = top
      top = bottom
      bottom = aux
    }
  }

  def inside(x: Int, y: Int): Boolean = x >= left && x <= right && y >= top && y <= bottom

  def move(dx: Int, dy: Int): Unit = {
    right += dx
    left += dx
    top += dy
    bottom += dy
  }

  def getData: String = left + " " + top + " " + right + " " + bottom

  def getCenterX: Float = (right + left) / 2f

  def getCenterY: Float = (top + bottom) / 2f

  def width: Int = right - left + 1

  def height: Int = bottom - top + 1
}