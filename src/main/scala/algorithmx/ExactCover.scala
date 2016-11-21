package algorithmx

object ExactCover {

  def solve[Col, Row](m: Matrix[Col, Row])(implicit ev: Ordering[Row]): Seq[Seq[Row]] =
    solveRecursively(m, Nil, Nil)(ev)

  private def solveRecursively[Col, Row](m: Matrix[Col, Row],
                                         solutions: Seq[Seq[Row]],
                                         partialSolution: Seq[Row])
                                        (implicit ev: Ordering[Row]): Seq[Seq[Row]] =
    if (m.isEmpty) {
      val solution = partialSolution.sorted(ev)
      solutions :+ solution
    }
    else for {
      c <- Seq(selectColumn(m))
      numOccurrences <- Seq(m.occurrences(c))
      if numOccurrences > 0
      r <- m.getRows(c)
      partialSolution2 = partialSolution ++ Seq(r)
      m2 = cover(m, r)
      s <- solveRecursively(m2, solutions, partialSolution2)
    } yield s

  private def selectColumn[Col, Row](m: Matrix[Col, Row]): Col = m.getColumnsSorted.head

  private def cover[Col, Row](m: Matrix[Col, Row], r: Row): Matrix[Col, Row] = {
    val columnsToDelete = m.getCols(r)
    val rowsToDelete = (columnsToDelete flatMap m.getRows).distinct
    m
      .deleteRows(rowsToDelete)
      .deleteCols(columnsToDelete)
  }
}
