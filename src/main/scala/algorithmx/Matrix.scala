package algorithmx

object Matrix {

  type Col = Int
  type Row = Int
  type Matrix = Seq[(Col, Seq[Row])]

  def getColumns(m: Matrix): Seq[Col] =
    m map (_._1)

  def getRows(m: Matrix, column: Col): Seq[Row] =
    (for {
      (c, rs) <- m
      if c == column
    } yield rs).head

  def getCols(m: Matrix, row: Row): Seq[Col] =
    for {
      (c, rs) <- m
      if rs contains row
    } yield c

  def deleteCols(columns: Seq[Col], m: Matrix): Matrix =
    for {
      t2 @ (c, rs) <- m
      if !(columns contains c)
    } yield t2

  def deleteRows(rows: Seq[Row], m: Matrix): Matrix =
    m map { case (c, rs) => (c, rs diff rows) }

  def occurences(m: Matrix, columnIndex: Col): Int = {
    val columns = m filter { case (c, _) => c == columnIndex }
    val column = columns.head._2
    column.length
  }

  def getColumnsSorted(m: Matrix): Seq[Col] = {
    val counts = m map (_._2.length)
    val columns = getColumns(m)
    val columnToCount = columns zip counts
    val sortedColumnToCount = columnToCount sortWith least
    sortedColumnToCount map (_._1)
  }

  private def least(t1: (Col, Int), t2: (Col, Int)): Boolean =
    (t1, t2) match {
      case ((_, a), (_, b)) if a < b => true
      case ((_, a), (_, b)) if a > b => false
      case ((x, a), (y, b)) if a == b && x < y => true
      case ((x, a), (y, b)) if a == b && x > y => false
      case _ => false
    }
}
