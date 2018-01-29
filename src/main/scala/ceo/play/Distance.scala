package ceo.play

class Distance private(val rowDiff: Int, val columnDiff: Int) {

  override def toString: String = s"($rowDiff, $columnDiff)"

  def toUnitVector: Distance = {
    val unitRow = if (rowDiff > 0) 1 else if (rowDiff < 0) -1 else 0
    val unitColumn = if (columnDiff > 0) 1 else if (columnDiff < 0) -1 else 0
    Distance(unitRow, unitColumn)
  }

  @inline final def +(other: Distance): Distance = Distance(rowDiff + other.rowDiff, columnDiff + other.columnDiff)

  @inline final def +(other: BoardPos): BoardPos = BoardPos(rowDiff + other.row, columnDiff + other.column)

  @inline final def -(other: Distance): Distance = Distance(rowDiff - other.rowDiff, columnDiff - other.columnDiff)

  @inline final def *(multiplier: Int): Distance = Distance(rowDiff * multiplier, columnDiff * multiplier)

  def setRow(newRow: Int) = Distance(newRow, columnDiff)

  def setColumn(newColumn: Int) = Distance(rowDiff, newColumn)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Distance]

  override def equals(other: Any): Boolean = other match {
    case that: Distance =>
      (that canEqual this) &&
        rowDiff == that.rowDiff &&
        columnDiff == that.columnDiff
    case _ => false
  }

  override def hashCode(): Int = rowDiff + 7 * 15 + (columnDiff + 7)
}

object Distance {
  private final val cache: Array[Distance] = (for {
    row <- -7 to 7
    column <- -7 to 7
  } yield new Distance(row, column)).toArray

  def apply(rowDiff: Int, columnDiff: Int): Distance = {
    cache((rowDiff + 7) * 15 + (columnDiff + 7))
  }

  def getIfValid(rowDiff: Int, columnDiff: Int): Distance = {
    if (rowDiff < -7 || rowDiff > 7 || columnDiff < -7 || columnDiff > 7) {
      throw new Exception(s"Distance object out of bounds ($rowDiff, $columnDiff)")
    } else {
      cache((rowDiff + 7) * 15 + (columnDiff + 7))
    }
  }

  val adjacentDistances: List[Distance] = (for {
    y <- -1 to 1
    x <- -1 to 1
    if x != 0 || y != 0
  } yield Distance(y, x)).toList
}
