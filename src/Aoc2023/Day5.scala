package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day5 {
  val input = Source.fromFile("data2023/5").mkString.split("\n\n")

  abstract class Mapper {
    def apply(in: BigInt): BigInt
  }

  object IdentityMapper extends Mapper {
    override def apply(in: BigInt): BigInt = in
  }

  case class RangeMapper(dst: BigInt, src: BigInt, len: BigInt, parent: Mapper) extends Mapper {
    override def apply(in: BigInt): BigInt =
      if (in >= src && in < src + len) dst + (in - src) else parent(in)
  }

  val seeds = input
    .head
    .split("\n")
    .head
    .split(": ")(1)
    .split(" ").map(BigInt(_)).toList

  val maps = input.tail.map { section =>
    val _ :: conversions = section.split("\n").toList
    conversions.reverse.foldLeft(IdentityMapper: Mapper) { case (state, line) =>
      val numbers = line.split(" ").map(BigInt(_))
      RangeMapper(numbers(0), numbers(1), numbers(2), state)
    }
  }

  def seedLocation(seed: BigInt) = maps.foldLeft(seed) { (current, m) => m(current) }

  val part1 = seeds.map(seedLocation).min

  val part2 = seeds.grouped(2).flatMap { case (start :: len :: _) => start to start + len }.map(seedLocation).min
}

object Day5Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day5.part1)
    println(LocalDateTime.now())
  }
}
