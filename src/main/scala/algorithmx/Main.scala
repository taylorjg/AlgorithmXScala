package algorithmx

import ExactCover._

object Main {
  def main(args: Array[String]): Unit = {

    val matrix1 = new Matrix[Int, Int] {
      override val m = Seq(
        1 -> Seq(1, 2),
        2 -> Seq(5, 6),
        3 -> Seq(4, 5),
        4 -> Seq(1, 2, 3),
        5 -> Seq(3, 4),
        6 -> Seq(4, 5),
        7 -> Seq(1, 3, 5, 6)
      )
    }
    println(s"simpleMatrix: $matrix1")
    val solution1 = solve(matrix1)
    println(s"solution1: $solution1")

    val matrix2 = new Matrix[Int, Char] {
      override val m = Seq(
        1 -> "AB",
        2 -> "EF",
        3 -> "DE",
        4 -> "ABC",
        5 -> "CD",
        6 -> "DE",
        7 -> "ACEF"
      )
    }
    println(s"simpleMatrix: $matrix2")
    val solution2 = solve(matrix2)
    println(s"solution2: $solution2")

    val matrix3 = new Matrix[Int, Int] {
      override val m = Seq(
        1 -> Seq(1, 3),
        2 -> Seq(2, 4),
        3 -> Seq(0, 2),
        4 -> Seq(1, 3, 5),
        5 -> Seq(0, 5),
        6 -> Seq(0, 2),
        7 -> Seq(1, 4, 5)
      )
    }
    val solution3 = solve(matrix3)
    println(s"solution3: $solution3")

    val matrix4 = new Matrix[Int, Int] {
      override val m = Seq(
        1 -> Seq(0, 2),
        2 -> Seq(1, 4),
        3 -> Seq(1, 3, 5),
        4 -> Seq(2, 3)
      )
    }
    val solution4 = solve(matrix4)
    println(s"solution4: $solution4")
  }
}
