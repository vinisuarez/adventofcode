package day3

import scala.annotation.tailrec
import scala.io.Source

object BinaryDiagnostic2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day3/input.txt")
    val input = str.getLines().toSeq

    val oxygen = findValue(input, 0, '1')
    val co2 = findValue(input, 0, '0')

    println(Integer.parseInt(oxygen, 2) * Integer.parseInt(co2, 2))
  }

  @tailrec
  def findValue(filteredList: Seq[String], pos: Int, favoriteValue: Char): String = {
    if (filteredList.size == 1) filteredList.head
    else findValue(filterListForPosition(filteredList, pos, favoriteValue), pos + 1, favoriteValue)
  }

  def filterListForPosition(filteredList: Seq[String], pos: Int, favoriteValue: Char): Seq[String] = {
    val oneCount = filteredList.map(_.charAt(pos).toString.toInt).sum
    val zeroCount = filteredList.size - oneCount
    if (oneCount >= zeroCount) filteredList.filter(_.charAt(pos).equals(favoriteValue))
    else filteredList.filterNot(_.charAt(pos).equals(favoriteValue))
  }
}
