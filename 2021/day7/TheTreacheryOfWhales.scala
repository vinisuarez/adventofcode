package day7

import scala.io.Source

object TheTreacheryOfWhales {

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day7/input.txt")
    val input = str.getLines().flatten(v => v.split(',')).map(_.toInt).toSeq

    println(findBestFuel(input, 0, input.max, Int.MaxValue))
  }

  private def findBestFuel(input: Seq[Int], currentPosition:Int, maxPosition: Int, bestConsumption: Int): Int = {
    if (currentPosition == maxPosition) bestConsumption
    else {
      val fuelConsumption = calculateFuel(input, currentPosition)
      if (fuelConsumption < bestConsumption) findBestFuel(input, currentPosition + 1, maxPosition, fuelConsumption)
      else findBestFuel(input, currentPosition + 1, maxPosition, bestConsumption)
    }
  }

  private def calculateFuel(input: Seq[Int], position: Int): Int = {
    input.map(i => {
      if (i == position) 0
      else if (i > position) i - position
      else position - i
    }).sum
  }
}
