package day8

import scala.io.Source

object SevenSegmentSearch2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day8/input.txt")
    val lines = str.getLines().map(_.split('|')).toSeq
    println(lines.flatten(i => {
      val inputLine = i.head.split("\\s+").groupBy(_.length)
      val positions = findPositions(inputLine, Array.fill[String](7)(""))
      i.tail.map(o => decode(o, positions))
    }).sum)
  }

  private def findPositions(inputs: Map[Int, Array[String]], positions: Array[String]): Array[String] = {
    val with2 = inputs.getOrElse(2, Array()) // 1
    val with3 = inputs.getOrElse(3, Array()) // 7
    val with4 = inputs.getOrElse(4, Array()) // 4
    val with5 = inputs.getOrElse(5, Array()) // 2, 3, 5
    val with6 = inputs.getOrElse(6, Array()) // 0, 6, 9

    positions(0) = with3.head.filterNot(ch => with2.head.contains(ch))
    positions(1) = with4.head.filterNot(ch => with2.head.contains(ch))
    positions(3) = with4.head.filterNot(ch => with2.head.contains(ch))

    with6.foreach(w => {
      val diff = with4.head.filterNot(ch => w.contains(ch))
      if (diff.isEmpty) {
        val potential = w.filter(ch2 => !with4.head.contains(ch2))
        if (positions(0).nonEmpty) {
          positions(6) = potential.filterNot(ch => positions(0).contains(ch))
        }
      } else {
        // either 2 or 3
        if (positions(3).nonEmpty && positions(3).contains(diff)) {
          positions(3) = diff
          if (positions(1).nonEmpty && positions(1).contains(diff)) {
            positions(1) = positions(1).filterNot(ch => diff.contains(ch))
          }
        } else if (positions(2).nonEmpty && positions(2).contains(diff)) {
          positions(2) = diff
        }
      }
    })

    with5.foreach(w => {
      val diff = with4.head.filterNot(ch => w.contains(ch))
      if (diff.length == 2) {
        // either 1 and 5
        if (positions(1).nonEmpty) {
          if (!positions(1).contains(diff)) {
            positions(5) = diff.filterNot(ch => positions(1).contains(ch))
          }
        }
      } else {
        // either 1 or 2
        if (positions(1).isEmpty || positions(1).contains(diff)) {
          positions(1) = diff
        } else if (positions(2).isEmpty || positions(2).contains(diff)) {
          positions(2) = diff
        }
      }
    })

    val missing = "abcdefg".filterNot(ch => positions.mkString("").contains(ch))
    val i = positions.indexOf("")
    positions(i) = missing
    positions
  }

  private def decode(outputs: String, knownPosition: Array[String]): Int = {
    outputs.trim.split("\\s+").map(output => {
      val value = output.map(i => knownPosition.indexOf(i.toString)).mkString
      val sorted = value.toSeq.sortWith(_ < _).unwrap

      if (sorted.equals("012456")) "0"
      else if (sorted.equals("25")) "1"
      else if (sorted.equals("02346")) "2"
      else if (sorted.equals("02356")) "3"
      else if (sorted.equals("1235")) "4"
      else if (sorted.equals("01356")) "5"
      else if (sorted.equals("013456")) "6"
      else if (sorted.equals("025")) "7"
      else if (sorted.equals("0123456")) "8"
      else if (sorted.equals("012356")) "9"
    }).mkString.toInt
  }
}
