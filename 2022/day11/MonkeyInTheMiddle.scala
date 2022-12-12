package day11

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object MonkeyInTheMiddle {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day11/input.txt")
    val monkeys = str.getLines().toSeq.grouped(7).map(i => parseToMonkey(i)).toSeq
    val value = run(collection.mutable.Map() ++ monkeys.map(m => m.id -> m).toMap, 0).values.map(_.count.toLong).toSeq.sorted(Ordering.Long.reverse)
    println(value.head * value.tail.head)
  }

  @tailrec
  def run(monkeyMap: mutable.Map[Int, Monkey], turn: Int): mutable.Map[Int, Monkey] = {
    if (turn == 10000) monkeyMap
    else {
      monkeyMap.values.foreach(monkey => {
        monkey.count += monkey.items.length
        monkey.items.foreach(i => {
          val worry = operation(monkey.operation, i) % monkeyMap.values.map(_.test).product
          val otherMonkey = if (worry % monkey.test == 0) monkeyMap(monkey.trueMonkey) else monkeyMap(monkey.falseMonkey)
          otherMonkey.items = otherMonkey.items :+ worry
          monkeyMap(otherMonkey.id) = otherMonkey
        })
        monkey.items = Seq()
      })
      run(monkeyMap, turn + 1)
    }
  }
  case class Monkey(id: Int, var items: Seq[Long], operation: String, test: Int, trueMonkey: Int, falseMonkey: Int, var count: Int)

  def parseToMonkey(input: Seq[String]): Monkey = {
    Monkey(
      id = input.head.replace("Monkey ", "").head.toString.toInt,
      items = input(1).replace("  Starting items: ", "").split(",").map(_.trim.toLong),
      operation = input(2).replace("  Operation: new = old ", ""),
      test = input(3).replace("  Test: divisible by ", "").toInt,
      trueMonkey = input(4).replace("    If true: throw to monkey ", "").toInt,
      falseMonkey = input(5).replace("    If false: throw to monkey ", "").toInt,
      count = 0
    )
  }

  def operation(input: String, old: Long): Long = {
    val split = input.split("\\s+")
    val value = if (split(1).equals("old")) old else split(1).toInt
    split(0) match {
      case "*" => old * value
      case "/" => old / value
      case "+" => old + value
      case "-" => old - value
    }
  }
}