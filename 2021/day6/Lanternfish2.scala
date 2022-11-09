package day6

import scala.annotation.tailrec
import scala.io.Source

object Lanternfish2 {

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day6/input.txt")
    val input = (0 to 9).map(v => v -> 0L).toMap ++
      str.getLines().flatten(v => v.split(',')).map(_.toInt).toSeq.groupBy(_.intValue).map(v => (v._1, v._2.size.toLong))

    println(run(input, day = 0))
  }

  @tailrec
  def run(dayByFishCount: Map[Int, Long], day: Int): Long =
    if (day == 256) dayByFishCount.values.sum
    else run(updateList(dayByFishCount, dayByFishCount.getOrElse(0, 0L)), day + 1)

  private def updateList(dayByFishCount: Map[Int, Long], day0Count: Long): Map[Int, Long] = {
    dayByFishCount.map(entry => {
      val day = entry._1
      val nextDayCount = dayByFishCount.getOrElse(day + 1, 0L)
      if (day == 6) (day, nextDayCount + day0Count)
      else (day, nextDayCount)
    }) ++ Map((8, day0Count))
  }
}