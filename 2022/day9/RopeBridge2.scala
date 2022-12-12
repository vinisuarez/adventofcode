package day9

import scala.io.Source

object RopeBridge2 {
  val range: Seq[Int] = 1 to 9

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day9/input.txt")
    val input = str.getLines().toSeq
    println(run(input, Array.fill(10)((100, 100)), Seq()))
  }

  def run(input: Seq[String], positions: Array[(Int, Int)], visitPositions: Seq[(Int, Int)]): Int = {
    if (input.isEmpty) visitPositions.distinct.size
    else {
      val split = input.head.split("\\s+")
      val value = split(1).toInt

      split(0) match {
        case "R" =>
          val visited = (0 until value).map(_ => {
            positions(0) = (positions(0)._1 + 1, positions(0)._2)
            range.foreach(i => positions(i) = newPosition(positions(i), positions(i - 1)))
            positions(9)
          })
          run(input.tail, positions, visitPositions ++ visited.distinct)
        case "L" =>
          val visited = (0 until value).map(_ => {
            positions(0) = (positions(0)._1 - 1, positions(0)._2)
            range.foreach(i => positions(i) = newPosition(positions(i), positions(i - 1)))
            positions(9)
          })
          run(input.tail, positions, visitPositions ++ visited.distinct)
        case "U" =>
          val visited = (0 until value).map(_ => {
            positions(0) = (positions(0)._1, positions(0)._2 - 1)
            range.foreach(i => positions(i) = newPosition(positions(i), positions(i - 1)))
            positions(9)
          })
          run(input.tail, positions, visitPositions ++ visited.distinct)
        case "D" =>
          val visited = (0 until value).map(_ => {
            positions(0) = (positions(0)._1, positions(0)._2 + 1)
            range.foreach(i => positions(i) = newPosition(positions(i), positions(i - 1)))
            positions(9)
          })
          run(input.tail, positions, visitPositions ++ visited.distinct)
      }
    }
  }

  private def newPosition(current: (Int, Int), previous: (Int, Int)): (Int, Int) = {
    if ((previous._1 - current._1).abs > 1)
      (
        if (previous._1 - current._1 > 0) current._1 + 1 else current._1 - 1,
        if (previous._2 - current._2 == 0) current._2 else if (previous._2 - current._2 > 0) current._2 + 1 else current._2 - 1
      )
    else if ((previous._2 - current._2).abs > 1)
      (
        if (previous._1 - current._1 == 0) current._1 else if (previous._1 - current._1 > 0) current._1 + 1 else current._1 - 1,
        if (previous._2 - current._2 > 0) current._2 + 1 else current._2 - 1
      )
    else
      current
  }
}