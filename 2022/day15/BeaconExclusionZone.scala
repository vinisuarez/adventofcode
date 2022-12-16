package day15

import scala.io.Source

object BeaconExclusionZone {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day15/input.txt")
    val input = str.getLines().map(i => parse(i)).toSeq

    val first = input.map(i => {
      val distance = (i._1 - i._3).abs + (i._2 - i._4).abs - (i._2 - 2000000).abs
      (i._1 - distance, i._1 + distance)
    }).filter(i => i._1 <= i._2)
    println(run(first.sortBy(_._1)).map(r => r._2 - r._1).sum)

  }
  def parse(input: String): (Int, Int, Int, Int) = {
    val split = input
      .replace("Sensor at x=", "")
      .replace(": closest beacon is at x=", ",")
      .replace(" y=", "")
      .split(",")
    (split(0).toInt, split(1).toInt, split(2).toInt, split(3).toInt)
  }

  def run(ranges: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    var simplified = Array(ranges.head)
    (1 until ranges.size).foreach(i => {
      val current = ranges(i)
      val last = simplified.last

      if (current._1 <= last._2 + 1) {
        simplified(simplified.length - 1) = (last._1, Math.max(last._2, current._2))
      } else {
        simplified = simplified :+ current
      }
    })
    simplified
  }
}