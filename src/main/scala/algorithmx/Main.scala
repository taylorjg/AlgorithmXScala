package algorithmx

import ExactCover._

object Main {
  def main(args: Array[String]): Unit = {

    val simpleMatrix = Seq(
      1 -> Seq(1, 2), // AB
      2 -> Seq(5, 6), // EF
      3 -> Seq(4, 5), // DE
      4 -> Seq(1, 2, 3), // ABC
      5 -> Seq(3, 4), // CD
      6 -> Seq(4, 5), // DE
      7 -> Seq(1, 3, 5, 6)) // ACEF
    println(s"simpleMatrix: $simpleMatrix")

    val solution = solve(simpleMatrix)
    println(s"solution: $solution")
  }
}
