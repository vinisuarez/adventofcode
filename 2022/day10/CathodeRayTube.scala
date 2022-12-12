package day10

import scala.io.Source

object CathodeRayTube {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day10/input.txt")
    val input = str.getLines().toSeq
    println(run(input, Map(), 1, 1))
  }

  def run(input: Seq[String], signals: Map[Int, Int], currentPos: Int, currentX: Int): Int = {
    if (input.isEmpty) {
      drawScreen(signals)
      20 * signals(20) + 60 * signals(60) + 100 * signals(100) + 140 * signals(140) + 180 * signals(180) + 220 * signals(220)
    } else {
      val split = input.head.split("\\s+")
      split(0) match {
        case "noop" =>
          run(input.tail, signals ++ Map(currentPos -> currentX), currentPos + 1, currentX)
        case "addx" =>
          val newX = currentX + split(1).toInt
          run(input.tail, signals ++ Map(currentPos -> currentX, currentPos + 1 -> currentX, currentPos + 2 -> newX), currentPos + 2, newX)
      }
    }
  }

  def drawScreen(signals: Map[Int, Int]): Unit = {
    val screen  = Array.ofDim[String](6, 40)
    (1 to 40).foreach(i => buildScreen(signals, screen, 0, i))
    (41 to 80).foreach(i => buildScreen(signals, screen, 1, i))
    (81 to 120).foreach(i => buildScreen(signals, screen, 2, i))
    (121 to 160).foreach(i => buildScreen(signals, screen, 3, i))
    (161 to 200).foreach(i => buildScreen(signals, screen, 4, i))
    (201 to 240).foreach(i => buildScreen(signals, screen, 5, i))
    screen.foreach(row => println(row.mkString("")))
  }

  private def buildScreen(signals: Map[Int, Int], screen: Array[Array[String]], r: Int, c: Int): Unit = {
    val x = signals(c)
    val column = (c - 1) - (40 * r)
    if (Seq(x - 1, x, x + 1).contains(column)) screen(r)(column) = "#"
    else screen(r)(column) = "."
  }
}