package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day8 {
  val input = Source.fromFile("data2023/8").getLines()

  val instructions = input.next()
  input.next()
  val states = input.map { row =>
    val Array(name, rest) = row.split(" = \\(")
    val Array(left, right) = rest.split(", |\\)")
    (name, Array(left, right))
  }.toMap

  def runInstructions(steps: Int, start: String, path: String): Either[(String, Int), Int] = {
    path.foldLeft(Left((start, steps)): Either[(String, Int), Int]) { case (state, instr) =>
      if (state.isLeft) {
        val next = states(state.left.get._1)(if (instr == 'L') 0 else 1)
        if (next == "ZZZ")
          Right(state.left.get._2)
        else
          Left(next, state.left.get._2 + 1)
      } else
        state
    }
  }

  def search(steps: Int, start: String): Int = {
    runInstructions(steps, start, instructions) match {
      case Left((pos, n)) => search(n, pos)
      case Right(n) => n + 1
    }
  }

  val part1 = search(0, "AAA")

  def runInstructions2(steps: Int, start: String, path: String): Either[(String, Int), Int] = {
    path.foldLeft(Left((start, steps)): Either[(String, Int), Int]) { case (state, instr) =>
      if (state.isLeft) {
        val next = states(state.left.get._1)(if (instr == 'L') 0 else 1)
        if (next.endsWith("Z"))
          Right(state.left.get._2)
        else
          Left(next, state.left.get._2 + 1)
      } else
        state
    }
  }

  def search2(steps: Int, start: String): Int = {
    runInstructions2(steps, start, instructions) match {
      case Left((pos, n)) => search2(n, pos)
      case Right(n) => n + 1
    }
  }

  def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / a.gcd(b)

  def lcmOfList(numbers: List[BigInt]): BigInt = numbers.foldLeft(BigInt(1))(lcm)

  val part2 = lcmOfList(states.keys.filter(_.endsWith("A")).map(start => search2(0, start)).map(BigInt(_)).toList)
}

object Day8Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day8.part1)
    println(LocalDateTime.now())
  }
}
