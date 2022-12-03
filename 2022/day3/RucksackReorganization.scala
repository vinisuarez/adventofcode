package day3

import scala.annotation.tailrec
import scala.io.Source

object RucksackReorganization {
  val alphabet = 'a' to 'z'

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day3/input.txt")
    val input = str.getLines().toSeq
    println(run(input, Seq()).sum)
    println(run2(input, Seq()).sum)
  }

  @tailrec
  def run(items: Seq[String], priorities: Seq[Int]): Seq[Int] = {
    if (items.isEmpty) priorities
    else {
      val item = items.head
      val (first, second) = item.splitAt(item.length / 2)
      val itemType = first.toSeq.intersect(second).unwrap.head
      if (itemType.isUpper)
        run(items.tail, priorities :+ alphabet.indexOf(itemType.toLower) + 27)
      else
        run(items.tail, priorities :+ alphabet.indexOf(itemType) + 1)
    }
  }

  @tailrec
  def run2(items: Seq[String], badges: Seq[Int]): Seq[Int] = {
    if (items.isEmpty) badges
    else {
      val itemGroup = items.take(3)
      val itemType = itemGroup.head.toSeq.intersect(itemGroup.tail.head).intersect(itemGroup(2)).unwrap.head
      if (itemType.isUpper)
        run2(items.drop(3), badges :+ alphabet.indexOf(itemType.toLower) + 27)
      else
        run2(items.drop(3), badges :+ alphabet.indexOf(itemType) + 1)
    }
  }
}
