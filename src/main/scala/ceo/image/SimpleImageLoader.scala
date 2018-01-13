package ceo.image

trait SimpleImageLoader {

  def getImageAt(row: Int, column: Int): PieceImage

}