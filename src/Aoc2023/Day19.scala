package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day19 {
  case class Condition(name: Char, op: Char, value: Int, dest: String)
  case class Workflow(name: String, conditions: List[Condition], goal: String)

  val input = Source.fromFile("data2023/19").mkString

  val (workflows, states) = input.split("\n\n") match {
    case Array(conditions, states) =>
      (conditions.split("\n").map(_.split("[{}]"))
        .map { case Array(name, conds) =>
          val goal :: condParts = conds.split(",").reverse.toList
          val conditions = condParts.reverse.map { case part =>
            val Array(condition, dest) = part.split(":")
            Condition(condition(0), condition(1), condition.substring(2).toInt, dest)
          }
          (name, Workflow(name, conditions, goal))
        }.toMap,
        states.split("\n").map(_.replace("{", "")
          .replace("}", "")
          .split(",").map(binding => binding(0) -> binding.substring(2).toInt))
          .map(_.toMap)
      )
  }

  def evaluateCondition(condition: Condition, bindings: Map[Char, Int]) = condition.op match {
    case '<' => bindings(condition.name) < condition.value
    case '>' => bindings(condition.name) > condition.value
  }

  @tailrec
  def evaluate(bindings: Map[Char, Int], workflowName: String = "in"): Option[Int] = {
    val workflow = workflows(workflowName)
    val result = workflow.conditions.find(condition => evaluateCondition(condition, bindings)).map(_.dest).getOrElse(workflow.goal)
    result match {
      case "A" => Some(bindings.values.sum)
      case "R" => None
      case next => evaluate(bindings, next)
    }
  }

  lazy val part1 = states.toList.flatMap(bindings => evaluate(bindings)).sum

  case class Range(from: Int, to: Int) {
    def splitRange(op: Char, value: Int) = op match {
      case '>' if value > from && value < to => (Range(value+1, to), Range(from, value))
      case '>' if to <= value => (EmptyRange, Range(from, to))
      case '>' if from > value => (Range(from, to), Range(from, to))
      case '<' if value > from && value < to => (Range(from, value-1), Range(value, to))
      case '<' if to <= value => (Range(from, to), EmptyRange)
      case '<' if from < value => (Range(from, to), Range(from, to))
    }
    val size: BigInt = to - from + 1
  }
  object EmptyRange extends Range(0, 0)

  def evaluate2conditions(bindings: Map[Char,Range], conditions: List[Condition], default: String): List[Map[Char, Range]] = conditions match {
    case Nil => evaluate2(bindings, default)
    case cond :: conds =>
      val (t, f) = bindings(cond.name).splitRange(cond.op, cond.value)
      val truthy = if (t == EmptyRange) Nil else evaluate2(bindings + (cond.name -> t), cond.dest)
      val falsy = if (f == EmptyRange) Nil else evaluate2conditions(bindings + (cond.name -> f), conds, default)
      truthy ++ falsy
  }

  def evaluate2(bindings: Map[Char, Range], workflowName: String = "in"): List[Map[Char, Range]] = {
    workflowName match {
      case "A" => List(bindings)
      case "R" => Nil
      case _ =>
        val workflow = workflows(workflowName)
        evaluate2conditions(bindings, workflow.conditions, workflow.goal)
    }
  }

  val startRangeBinding = "xmas".map(c => (c, Range(1, 4000))).toMap

  lazy val part2 = evaluate2(startRangeBinding).map(_.valuesIterator.map(_.size).product).sum
}

object Day19Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day19.part2)
    println(LocalDateTime.now())
  }
}
