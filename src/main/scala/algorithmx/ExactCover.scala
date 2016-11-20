package algorithmx

import Matrix._

object ExactCover {

  def solve(m: Matrix): Seq[Row] =
    solveHelper(m, Seq())

  private def solveHelper(m: Matrix, solution: Seq[Row]): Seq[Row] = {
    m match {
      case _ if m.nonEmpty =>
        val c = selectColumn(m)
        if (occurences(m, c) > 0) {
          for {
            r <- getRows(m, c)
            solution2 = solution ++ Seq(r)
            m2 = cover(m, r)
            s <- solveHelper(m2, solution2)
          } yield s
        }
        else {
          Seq()
        }
      case _ => solution
    }
  }

  private def selectColumn(m: Matrix): Col =
    getColumnsSorted(m).head

  private def cover(m: Matrix, r: Row): Matrix = {
    def rowsToDelete(row: Row) = {
      val v1 = getCols(m, row) map (getRows(m, _))
      val v2 = v1.flatten
      val v3 = v2.distinct
      v3
    }
    def columnsToDelete(row: Row) = getCols(m, row)
    val v1 = rowsToDelete(r)
    val v2 = deleteRows(v1, m)
    val v3 = columnsToDelete(r)
    val v4 = deleteCols(v3, v2)
    v4
  }
}
