package day8

import scala.io.Source

object TreetopTreeHouse {
  case class Node(path: String, var value: Int, var childrenNode: Seq[String])

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day8/input.txt")
    val input = str.getLines().toSeq
    val maxX = input.length
    val maxY = input.head.length
    val board = input.flatten(_.grouped(1)).grouped(maxY).toSeq

    val result = board.zipWithIndex.flatten(x => x._1.zipWithIndex.map(y => {
      if (x._2 == 0 || x._2 == maxX - 1 || y._2 == 0 || y._2 == maxY - 1) {
        1
      } else {
        val left = Range(0, y._2).map(r => board(x._2)(r))
        val right = Range(y._2 + 1, maxY).map(r => board(x._2)(r))
        val top = Range(0, x._2).map(r => board(r)(y._2))
        val bottom = Range(x._2 + 1, maxX).map(r => board(r)(y._2))

        val currentTree = board(x._2)(y._2).toInt

        if (!top.exists(_.toInt >= currentTree) || !bottom.exists(_.toInt >= currentTree) ||
          !left.exists(_.toInt >= currentTree) || !right.exists(_.toInt >= currentTree))
          1
        else
          0
      }
    }))

    val result2 = board.zipWithIndex.flatten(x => x._1.zipWithIndex.map(y => {
      if (x._2 == 0 || x._2 == maxX - 1 || y._2 == 0 || y._2 == maxY - 1) {
        0
      } else {
        val left = Range(0, y._2).map(r => board(x._2)(r)).reverse
        val right = Range(y._2 + 1, maxY).map(r => board(x._2)(r))
        val top = Range(0, x._2).map(r => board(r)(y._2)).reverse
        val bottom = Range(x._2 + 1, maxX).map(r => board(r)(y._2))

        val currentTree = board(x._2)(y._2).toInt

        (if (top.exists(_.toInt >= currentTree)) top.indexWhere(_.toInt >= currentTree) + 1 else top.size) *
          (if (bottom.exists(_.toInt >= currentTree)) bottom.indexWhere(_.toInt >= currentTree) + 1 else bottom.size) *
          (if (left.exists(_.toInt >= currentTree)) left.indexWhere(_.toInt >= currentTree) + 1 else left.size) *
          (if (right.exists(_.toInt >= currentTree)) right.indexWhere(_.toInt >= currentTree) + 1 else right.size)
      }
    }))

    println(result.sum)
    println(result2.max)
  }
}
