package algorithmx

import ExactCover._

object Main {
  def main(args: Array[String]): Unit = {

    val matrix1 = new Matrix[Int, Int](Seq(
        1 -> Seq(1, 2),
        2 -> Seq(5, 6),
        3 -> Seq(4, 5),
        4 -> Seq(1, 2, 3),
        5 -> Seq(3, 4),
        6 -> Seq(4, 5),
        7 -> Seq(1, 3, 5, 6)))
    val solutions1 = solve(matrix1)
    println(s"solutions1: $solutions1")

    val matrix2 = new Matrix[Int, Char](Seq(
        1 -> "AB",
        2 -> "EF",
        3 -> "DE",
        4 -> "ABC",
        5 -> "CD",
        6 -> "DE",
        7 -> "ACEF"))
    val solutions2 = solve(matrix2)
    println(s"solutions2: $solutions2")

    val matrix3 = new Matrix[Int, Int](Seq(
        1 -> Seq(1, 3),
        2 -> Seq(2, 4),
        3 -> Seq(0, 2),
        4 -> Seq(1, 3, 5),
        5 -> Seq(0, 5),
        6 -> Seq(0, 2),
        7 -> Seq(1, 4, 5)))
    val solutions3 = solve(matrix3)
    println(s"solutions3: $solutions3")

    val matrix4 = new Matrix[Int, Int](Seq(
        1 -> Seq(0, 2),
        2 -> Seq(1, 4),
        3 -> Seq(1, 3, 5),
        4 -> Seq(2, 3)))
    val solutions4 = solve(matrix4)
    println(s"solutions4: $solutions4")

    val matrix5 = new Matrix[Int, Int](Seq(
      1 -> Seq(0, 2),
      2 -> Seq(1, 4),
      3 -> Seq(1, 3, 5),
      4 -> Seq(2, 3)))
    val solutions5 = solve(matrix5).take(1)
    println(s"solutions5: $solutions5")
  }
}
