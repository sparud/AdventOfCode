package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day22 {
  case class Pos(x: Int, y: Int) {
    def +(other: Pos) = Pos(x + other.x, y + other.y)
    def *(factor: Int) = Pos(x * factor, y * factor)
  }


  case class Poz(x: Int, y: Int, z: Int) {
    val flat = Pos(x, y)
    def sink = Poz(x, y, z-1)
    def raise = Poz(x, y, z+1)
  }

  object Poz {
    def parse(s: String) = s.split(",") match {
      case Array(x, y, z) => Poz(x.toInt, y.toInt, z.toInt)
    }
  }

  case class Block(a: Poz, b: Poz) {
    val area =
      (a.x to b.x).flatMap(x =>
        (a.y to b.y).map(y => Pos(x, y))).toSet
    val volume =
      (a.x to b.x).flatMap(x =>
        (a.y to b.y).flatMap(y =>
          (a.z to b.z).map(z => Poz(x, y, z)))).toSet
    val top = area.map(p => Poz(p.x, p.y, b.z))
    val bottom = area.map(p => Poz(p.x, p.y, a.z))

    def intersects(other: Block) = area.intersect(other.area)
    def sink = Block(a.sink, b.sink)
    def raise = Block(a.raise, b.raise)
  }

  val input = Source.fromFile("data2023/22").mkString.split("\n").map(_.split("~") match {
      case Array(left, right) => Block(Poz.parse(left), Poz.parse(right))
    }).toSet

  def empty(blocks: Set[Block], poz: Poz) = blocks.forall(!_.volume.contains(poz))

  def canSink(blocks: Set[Block], block: Block): Boolean =
    block.a.z > 1 && block.area.forall(p => empty(blocks, Poz(p.x, p.y, block.a.z-1)))

  @tailrec
  def fixPoint[A](v:A, f: A=>A): A = {
    val newV = f(v)
    if (newV == v)
      newV
    else
      fixPoint(newV, f)
  }

  def sinkAll(blocks: Set[Block]) =
    blocks.map(block => if (canSink(blocks, block)) block.sink else block)

  def blocksAbove(blocks: Set[Block], block: Block) = {
    val above = block.top.map(_.raise)
    blocks.filter(b => b.bottom.intersect(above).nonEmpty)
  }

  def blocksBelow(blocks: Set[Block], block: Block) = {
    val below = block.bottom.map(_.sink)
    blocks.filter(_.top.intersect(below).nonEmpty)
  }

  lazy val sinked = fixPoint(input, sinkAll)

  lazy val part1 = sinked.count(block =>
    blocksAbove(sinked, block).forall(aboveBlock =>
      blocksBelow(sinked, aboveBlock).size > 1
    )
  )

  def count(blocks: Set[Block], current: Set[Block]): Int = {
    val above = current.flatMap(blocksAbove(blocks, _))
    if (above.isEmpty)
      0
    else {
      val newBlocks = blocks -- current
      val next = above.filter(canSink(newBlocks, _))
      next.size + count(newBlocks, next)
    }
  }

  lazy val part2 = sinked.toList.map(block => count(sinked, Set(block))).sum
}

object Day22Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day22.part2)
    println(LocalDateTime.now())
  }
}
