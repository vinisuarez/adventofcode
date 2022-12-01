package day9

import scala.io.Source

object SmokeBasin2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day9/input.txt")
    val lines = str.getLines.toSeq
    val maxX = lines.length
    val maxY = lines.head.length
    val board = lines.flatten(_.grouped(1)).grouped(maxY).toSeq

    val result = board.zipWithIndex.flatten(x => x._1.zipWithIndex.map(y => {
      val value = findAdjacentLocations(x._2, y._2, maxX, maxY, board).map(_.toInt)
      val position = board(x._2)(y._2).toInt
      if (!value.exists(_ <= position))
        position + 1
      else
        0
    }))

    println(result.sum)
  }


  private def findAdjacentLocations(x: Int, y: Int, maxX: Int, maxY: Int, board: Seq[Seq[String]]): Seq[String] = {
    if (x == 0) {
      if (y == 0) {
        Seq(board.head(1), board(1).head)
      } else if (y == maxY - 1) {
        Seq(board.head(maxY - 1), board(1)(y))
      } else {
        Seq(board(x)(y - 1), board(x)(y + 1), board(1)(y))
      }
    } else if (x == maxX - 1) {
      if (y == 0) {
        Seq(board(x - 1).head, board(x)(1))
      } else if (y == maxY - 1) {
        Seq(board(x - 1)(y), board(x)(y - 1))
      } else {
        Seq(board(x)(y - 1), board(x)(y + 1), board(x - 1)(y))
      }
    }
    else if (y == 0) {
      Seq(board(x - 1)(y), board(x + 1)(y), board(x)(y + 1))
    } else if (y == maxY - 1) {
      Seq(board(x - 1)(y), board(x + 1)(y), board(x)(y - 1))
    } else {
      Seq(board(x - 1)(y), board(x + 1)(y), board(x)(y + 1), board(x)(y - 1))
    }
  }

}
