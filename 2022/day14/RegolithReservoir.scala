package day14

import scala.io.Source

object RegolithReservoir {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day14/input.txt")
    val input = str.getLines().toSeq

    val (tunnel, bottom) = buildTunnel(input, Array.fill(550, 10)("."), 0)
    runSand(tunnel, bottom)
    //println(tunnel.transpose.map(_.mkString("")).mkString("\n"))
  }

  def buildTunnel(input: Seq[String], tunnel: Array[Array[String]], bottom: Int): (Array[Array[String]], Int) = {
    if (input.isEmpty) (tunnel, bottom)
    else {
      var newBottom = bottom
      val coordinates = input.head.split(" -> ")
      (1 until coordinates.length).foreach(i => {
        val (x, y) = getXAndY(coordinates(i))
        val (xp, yp) = getXAndY(coordinates(i - 1))
        if (xp == x) {
          if (y > yp) {
            if (y > newBottom) newBottom = y
            (yp to y).foreach(yc =>tunnel(x)(yc) = "#")
          }
          else {
            if (yp > newBottom) newBottom = yp
            (y to yp).foreach(yc =>tunnel(x)(yc) = "#")
          }
        } else {
          if (xp > x) (x to xp).foreach(xc => tunnel(xc)(y) = "#")
          else (xp to x).foreach(xc => tunnel(xc)(y) = "#")
        }
      })
      buildTunnel(input.tail, tunnel, newBottom)
    }
  }

  def runSand(tunnel: Array[Array[String]], bottom: Int): Array[Array[String]] = {
    def run(x: Int, startCount: Int, isCheck: Boolean): Boolean = {
      var count = startCount
      while (true) {
        if (count > bottom) {
         println(tunnel.transpose.map(_.mkString("")).mkString("\n"))
          println(tunnel.map(i=> i.count(_.equals("o"))).sum)
          return false
        }
        if (tunnel(x)(count).equals(".")) count += 1
        else {
            val canRight = if (tunnel(x-1)(count) == ".") run(x - 1, count, true) else false
            if (canRight && isCheck) return true
            if (canRight) {
              count = 0
            } else {
              val canLeft = if (tunnel(x+1)(count) == ".") run(x + 1, count, true) else false
              if (canLeft && isCheck) return true
              if (canLeft) {
                count = 0
              } else {
                if (tunnel(x)(count - 1) == "o") {
                  count = 0
                } else {
                  if (tunnel(x-1)(count) == ".") {
                    return false
                  } else {
                    tunnel(x)(count - 1) = "o"
                    if (isCheck) return true
                    else count = 0
                  }
                }
              }
            }
        }
      }
      false
    }

    run(500, 0, false)
    tunnel
  }

  def getXAndY(coordinate: String): (Int, Int) = {
    val c = coordinate.split(",")
    (c(0).toInt, c(1).toInt)
  }
}