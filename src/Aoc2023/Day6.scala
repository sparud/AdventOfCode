package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day6 {
  val input = Source.fromFile("data2023/6").getLines()

  def parse() = input.next().split("[\t ]+").tail.map(_.toInt)

  val times = parse()
  val distances = parse()
  val tasks = times.zip(distances)

  def countOk(time: Int, distance: Int): Unit = {
    println(s"Time: $time, distance: $distance")
  }

  def getDistances(time: Int) = (1 until time).map(lt => (time - lt) * lt)

  val part1 = tasks.map { case (t, d) =>
    getDistances(t).count(_ > d)
  }.product

  @inline
  def getDistance(totalTime: Int, chargeTime: Int): BigInt =
    BigInt(1) * (totalTime - chargeTime) * chargeTime

  def find(totalTime: Int, bestDistance: BigInt) = {

    def divide_and_conquer(condition: BigInt => Boolean, start: Int, stop: Int): Int = {
      if (stop - start < 2)
        stop
      else {
        val middle = (start + stop) / 2
        val distance = getDistance(totalTime, middle)
        if (condition(distance))
          divide_and_conquer(condition, middle, stop)
        else
          divide_and_conquer(condition, start, middle)
      }
    }

    divide_and_conquer(_ > bestDistance, totalTime / 2, totalTime) - divide_and_conquer(_ <= bestDistance, 1, totalTime / 2)
  }

  val part2time = (times.map(_.toString)).mkString("").toInt
  val part2distance = BigInt(distances.map(_.toString).mkString(""))

  val part2 = find(part2time, part2distance)
}

object Day6Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day6.part1)
    println(LocalDateTime.now())
  }
}
