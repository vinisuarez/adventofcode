package day9

import scala.io.Source

object RopeBridge {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day9/input.txt")
    val input = str.getLines().toSeq
    println(run(input, (100, 100), (100, 100), Seq()))
  }

  def run(input: Seq[String], headPosition: (Int, Int), tailPosition: (Int, Int), visitPositions: Seq[(Int, Int)]): Int = {
    if (input.isEmpty) visitPositions.distinct.size
    else {
      val split = input.head.split("\\s+")
      val value = split(1).toInt
      split(0) match {
        case "R" =>
          val newHead = (headPosition._1 + value, headPosition._2)
          val tailPositions = findTailPosition(newHead, tailPosition)
          run(input.tail, newHead, tailPositions.last, visitPositions ++ tailPositions)
        case "L" =>
          val newHead = (headPosition._1 - value, headPosition._2)
          val tailPositions = findTailPosition(newHead, tailPosition)
          run(input.tail, newHead, tailPositions.last, visitPositions ++ tailPositions)
        case "U" =>
          val newHead = (headPosition._1, headPosition._2 - value)
          val tailPositions = findTailPosition(newHead, tailPosition)
          run(input.tail, newHead, tailPositions.last, visitPositions ++ tailPositions)
        case "D" =>
          val newHead = (headPosition._1, headPosition._2 + value)
          val tailPositions = findTailPosition(newHead, tailPosition)
          run(input.tail, newHead, tailPositions.last, visitPositions ++ tailPositions)
      }
    }
  }

  def findTailPosition(head: (Int, Int), tail: (Int, Int)): Seq[(Int, Int)] = {
    if ((head._1 - tail._1).abs == 1 && (head._2 - tail._2).abs == 1) Seq(tail)
    else {
      val xDiff = head._1 - tail._1
      val yDiff = head._2 - tail._2
      if (xDiff != 0 && yDiff == 0) {
        if (xDiff > 0)
          (tail._1 until tail._1 + (head._1 - tail._1)).map(i => (i, tail._2))
        else
          (tail._1 until tail._1 + (head._1 - tail._1)).by(-1).map(i => (i, tail._2))
      } else if (xDiff == 0 && yDiff != 0) {
        if (yDiff > 0)
          (tail._2 until tail._2 + (head._2 - tail._2)).map(i => (tail._1, i))
        else
          (tail._2 until tail._2 + (head._2 - tail._2)).by(-1).map(i => (tail._1, i))
      } else if (xDiff != 0) {
        if (xDiff.abs == 1) {
          if (yDiff > 0) {
            val first = (tail._1 + xDiff, tail._2 + 1)
            (first._2 until first._2 + (head._2 - first._2)).map(i => (first._1, i))
          } else {
            val first = (tail._1 + xDiff, tail._2 - 1)
            (first._2 until first._2 + (head._2 - first._2)).by(-1).map(i => (first._1, i))
          }
        } else if (yDiff.abs == 1) {
          if (xDiff > 0) {
            val first = (tail._1 + 1, tail._2 + yDiff)
            (first._1 until first._1 + (head._1 - first._1)).map(i => (i, first._2))
          } else {
            val first = (tail._1 - 1, tail._2 + yDiff)
            (first._1 until first._1 + (head._1 - first._1)).by(-1).map(i => (i, first._2))
          }
        } else
          Seq(tail)
      } else
        Seq(tail)
    }
  }
}