package algorithmx

import ExactCover._

object Main {
  def main(args: Array[String]): Unit = {

    // A = 1
    // B = 2
    // C = 3
    // D = 4
    // E = 5
    // F = 6
    val matrix1 = Seq(
      1 -> Seq(1, 2), // AB
      2 -> Seq(5, 6), // EF
      3 -> Seq(4, 5), // DE
      4 -> Seq(1, 2, 3), // ABC
      5 -> Seq(3, 4), // CD
      6 -> Seq(4, 5), // DE
      7 -> Seq(1, 3, 5, 6)) // ACEF
    println(s"simpleMatrix: $matrix1")
    val solution1 = solve(matrix1)
    println(s"solution1: $solution1")

    // 1 solution
    val matrix2 = Seq(
      1 -> Seq(1, 3),
      2 -> Seq(2, 4),
      3 -> Seq(0, 2),
      4 -> Seq(1, 3, 5),
      5 -> Seq(0, 5),
      6 -> Seq(0, 2),
      7 -> Seq(1, 4, 5))
    val solution2 = solve(matrix2)
    println(s"solution2: $solution2")

    // 3 solutions
    val matrix3 = Seq(
      1 -> Seq(0, 2),
      2 -> Seq(1, 4),
      3 -> Seq(1, 3, 5),
      4 -> Seq(2, 3))
    val solution3 = solve(matrix3)
    println(s"solution3: $solution3")
  }
}
