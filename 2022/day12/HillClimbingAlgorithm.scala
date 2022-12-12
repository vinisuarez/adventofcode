package day12

import scala.io.Source

object HillClimbingAlgorithm {
  val alphabet = 'a' to 'z'
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day12/input.txt")
    val input = str.getLines().toSeq
    val maxX = input.length
    val maxY = input.head.length
    val board = input.flatten(_.grouped(1)).grouped(maxY).toSeq

    val start = findPosition(board, "S")
    val objective = findPosition(board, "E")

    println(board)
  }


  def findPosition(table: Seq[Seq[String]], elem: String): (Int, Int) = {
    val row = table.indexWhere(_.contains(elem))
    if (row > -1) (row, table(row).indexOf(elem))
    else (-1, -1)
  }
}