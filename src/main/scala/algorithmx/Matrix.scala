package algorithmx

class Matrix[Col, Row](m: Seq[(Col, Seq[Row])])(implicit ev: Ordering[Col]) {

  def isEmpty: Boolean = m.isEmpty

  def getColumns: Seq[Col] = m map (_._1)

  def getRows(column: Col): Seq[Row] =
    (for {
      (c, rs) <- m
      if c == column
    } yield rs).head

  def getCols(row: Row): Seq[Col] =
    for {
      (c, rs) <- m
      if rs contains row
    } yield c

  def deleteCols(columns: Seq[Col]): Matrix[Col, Row] =
    new Matrix[Col, Row](
      for {
        t2@(c, rs) <- m
        if !(columns contains c)
      } yield t2)

  def deleteRows(rows: Seq[Row]): Matrix[Col, Row] =
    new Matrix[Col, Row](m map { case (c, rs) => (c, rs diff rows) })

  def occurrences(columnIndex: Col): Int = {
    val columns = m filter { case (c, _) => c == columnIndex }
    val column = columns.head._2
    column.length
  }

  def getColumnsSorted: Seq[Col] = {
    val counts = m map (_._2.length)
    val columns = getColumns
    val columnToCount = columns zip counts
    val sortedColumnToCount = columnToCount sortWith least
    sortedColumnToCount map (_._1)
  }

  private def least(t1: (Col, Int), t2: (Col, Int)): Boolean =
    (t1, t2) match {
      case ((_, len1), (_, len2)) if len1 < len2 => true
      case ((col1, len1), (col2, len2)) if len1 == len2 && ev.lt(col1, col2) => true
      case _ => false
    }
}
