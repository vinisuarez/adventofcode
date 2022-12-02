package day2

import scala.annotation.tailrec
import scala.io.Source

object RockPaperScissors {

  case class Answers(winAnswer: String, loseAnswer: String, drawAnswer:String)

  val answers: Map[String, Answers] = Map(
    "A" -> Answers("Y", "Z", "X"),
    "B" -> Answers("Z", "X", "Y"),
    "C" -> Answers("X", "Y", "Z")
  )

  val answerAndScoreMap: Map[String, Int] = Map(
    "Y" -> 2,
    "X" -> 1,
    "Z" -> 3
  )

  val lostPoints = 0
  val drawPoint = 3
  val winPoints = 6

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day2/input.txt")
    val input = str.getLines().toSeq
    println(run(input, 0))
    println(run2(input, 0))
  }

  @tailrec
  def run(plays: Seq[String], points: Int): Int = {
    if (plays.isEmpty) points
    else {
      val (myPlay, answer) = getMyPlayAndAnswer(plays)
      if (myPlay.equals(answer.winAnswer))
        run(plays.tail, points + answerAndScoreMap.getOrElse(myPlay, 0) + winPoints)
      else if (myPlay.equals(answer.drawAnswer))
        run(plays.tail, points + answerAndScoreMap.getOrElse(myPlay, 0) + drawPoint)
      else
        run(plays.tail, points + answerAndScoreMap.getOrElse(myPlay, 0) + lostPoints)
    }
  }

  @tailrec
  def run2(plays: Seq[String], points: Int): Int = {
    if (plays.isEmpty) points
    else {
      val (myPlay, answer) = getMyPlayAndAnswer(plays)
      if (myPlay.equals("X"))
        run2(plays.tail, points + answerAndScoreMap.getOrElse(answer.loseAnswer, 0) + lostPoints)
      else if (myPlay.equals("Z"))
        run2(plays.tail, points + answerAndScoreMap.getOrElse(answer.winAnswer, 0) + winPoints)
      else
        run2(plays.tail, points + answerAndScoreMap.getOrElse(answer.drawAnswer, 0) + drawPoint)
    }
  }

  private def getMyPlayAndAnswer(plays: Seq[String]): (String, Answers) = {
    val split = plays.head.split("\\s+")
    (split(1), answers(split(0)))
  }
}
