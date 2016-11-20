package algorithmx

import Matrix._

object ExactCover {

  def solve(m: Matrix): Seq[Row] =
    solveHelper(m, Seq())

  private def solveHelper(m: Matrix, solution: Seq[Row]): Seq[Row] = {
    m match {
      case _ if m.nonEmpty =>
        val c = selectColumn(m)
        if (occurrences(m, c) > 0) {
          for {
            r <- getRows(m)(c)
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
    def columnsToDelete = getCols(m)(_)
    def rowsToDelete(row: Row) = (getCols(m)(row) flatMap getRows(m)).distinct
    deleteCols(columnsToDelete(r), deleteRows(rowsToDelete(r), m))
  }
}
