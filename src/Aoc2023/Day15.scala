package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day15 {
  val input = Source.fromFile("data2023/15").mkString.split(",")

  def hash(s: String): Int = s.foldLeft(0) { case (acc, c) => (acc + c) * 17 % 256 }

  lazy val part1 = input.map(hash).toList.sum

  type Slot = (String, Int)
  type Boxes = Map[Int, List[Slot]]

  case class Operation(name: String, value: Option[Int])

  def parseOperation(op: String): Operation = op.split("[-=]") match {
    case Array(name, value) => Operation(name, Some(value.toInt))
    case Array(name) => Operation(name, None)
  }

  val operations = input.map(parseOperation)

  def step(boxes: Boxes, operation: Operation): Boxes = {
    val boxNo = hash(operation.name)
    val content = boxes.getOrElse(boxNo, List())
    val newContent: List[Slot] = operation.value match {
      case None => content.filterNot(_._1 == operation.name)
      case Some(v) => if (content.map(_._1).contains(operation.name))
        content.map(slot => if (slot._1 == operation.name) (operation.name, v) else slot)
      else
        content ++ List((operation.name, v))
    }
    boxes + (boxNo -> newContent)
  }

  lazy val part2 =
    operations.foldLeft(Map(): Boxes) { case (boxes, operation: Operation) =>
      step(boxes, operation)
    }.map({ case (boxNo, slots) =>
      slots.zipWithIndex.map { case (slot, slotNo) => (boxNo + 1) * (slotNo + 1) * slot._2 }.sum
    }).sum
}

object Day15Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day15.part2)
    println(LocalDateTime.now())
  }
}
