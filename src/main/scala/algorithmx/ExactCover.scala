package algorithmx

object ExactCover {

  def solve[Col, Row](m: Matrix[Col, Row]): Seq[Row] = solveHelper(m, Seq())

  private def solveHelper[Col, Row](m: Matrix[Col, Row], solution: Seq[Row]): Seq[Row] =
    if (m.isEmpty) solution
    else for {
      c <- Seq(selectColumn(m))
      numOccurrences <- Seq(m.occurrences(c))
      if numOccurrences > 0
      r <- m.getRows(c)
      solution2 = solution ++ Seq(r)
      m2 = cover(m, r)
      s <- solveHelper(m2, solution2)
    } yield s

  private def selectColumn[Col, Row](m: Matrix[Col, Row]): Col = m.getColumnsSorted.head

  private def cover[Col, Row](m: Matrix[Col, Row], r: Row): Matrix[Col, Row] = {
    val columnsToDelete = m.getCols(r)
    val rowsToDelete = (m.getCols(r) flatMap m.getRows).distinct
    m
      .deleteRows(rowsToDelete)
      .deleteCols(columnsToDelete)
  }
}
