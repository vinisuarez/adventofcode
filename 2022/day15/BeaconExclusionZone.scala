package day15

import scala.io.Source

object BeaconExclusionZone {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day15/input.txt")
    val input = str.getLines().toSeq

    val (sx, sy, bx, by) = parse(input.head)

    println((sx,sy))
    println((bx,by))
  }

  def parse(input: String): (Int, Int, Int, Int) = {
    val pattern = "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".r
    val (ax, ay, bx, by) = input.head match {
      case pattern(ax, ay, bx, by) => (ax.toInt, ay.toInt, bx.toInt, by.toInt)
    }
    (ax, ay, bx, by)
  }

  def distance(a: (Int, Int), b: (Int, Int)): Int = {
    (a._1 - b._1).abs + (a._2 - b._2).abs
  }
}