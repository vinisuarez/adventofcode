package day15

import scala.io.Source

object BeaconExclusionZone2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day15/input.txt")
    val input = str.getLines().map(i => parse(i)).toSeq
    val sensors = input.map(i => ((i._1, i._2), (i._1 - i._3).abs + (i._2 - i._4).abs))
    sensors.foreach(s => {
      (-(s._2 + 1) to s._2).foreach(i => {
        val distance = s._2 - i.abs
        if (check(sensors, (s._1._1 - distance - 1, s._1._2 + i)) ||
            check(sensors, (s._1._1 + distance + 1, s._1._2 + i))) {
          return
        }
      })
    })
  }

  private def check(sensors: Seq[((Int, Int), Int)], pos: (Int, Int)): Boolean = {
    if (pos._1 >= 0 &&
      pos._1 <= 4000000 &&
      pos._2 >= 0 &&
      pos._2 <= 4000000 &&
      sensors.forall(s => !(((pos._1 - s._1._1).abs + (pos._2 - s._1._2).abs) <= s._2))) {
      println(pos._1.toLong * 4000000 + pos._2.toLong)
      return true
    }
    false
  }

  def parse(input: String): (Int, Int, Int, Int) = {
    val split = input
      .replace("Sensor at x=", "")
      .replace(": closest beacon is at x=", ",")
      .replace(" y=", "")
      .split(",")
    (split(0).toInt, split(1).toInt, split(2).toInt, split(3).toInt)
  }
}