package day4

import scala.annotation.tailrec
import scala.io.Source

object CampCleanup {

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day4/input.txt")
    val input = str.getLines().toSeq
    println(run(input, 0, 0))
  }

  @tailrec
  def run(sections: Seq[String], fullyContainsCount: Int, intersectCount: Int): (Int, Int) = {
    if (sections.isEmpty) (fullyContainsCount, intersectCount)
    else {
      val (first, second) = getRanges(sections.head)
      run(
        sections.tail,
        fullyContainsCount + (if (first.subsetOf(second) || second.subsetOf(first)) 1 else 0),
        intersectCount + (if (first.intersect(second).nonEmpty || second.intersect(first).nonEmpty) 1 else 0)
      )
    }
  }

  private def getRanges(section: String) = {
    val values = section.split(",")
    val firstSplit = values(0).split("-")
    val secondSplit = values(1).split("-")
    (
      Range(firstSplit.head.toInt, firstSplit.tail.head.toInt + 1).toSet,
      Range(secondSplit.head.toInt, secondSplit.tail.head.toInt + 1).toSet
    )
  }
}
