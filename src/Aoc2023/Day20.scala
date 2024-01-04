package Aoc2023

import java.awt.BufferCapabilities.FlipContents
import java.time.LocalDateTime
import scala.io.Source
import scala.util.control.Breaks

object Day20 {
  abstract class Module(val targets: Seq[String]) {
    var signals = scala.collection.mutable.MutableList[Boolean]()
    def process(from: String, value: Boolean): Unit
    def addSource(from: String): Unit = {}
    def unTrigger = signals.clear()
    def get = {
      val res = signals.toList
      signals.clear()
      res
    }
    def current: Boolean
  }

  case class FlipFlop(override val targets: Seq[String]) extends Module(targets) {
    var state: Boolean = false
    def process(from: String, value: Boolean): Unit = {
      if (!value) {
        state = !state
        signals += state
      }
    }
    def current = state
  }

  case class Conjunction(name: String, override val targets: Seq[String]) extends Module(targets)  {
    var memory = scala.collection.mutable.HashMap[String, Boolean]()

    def process(from: String, value: Boolean): Unit = {
      memory(from) = value
      val newValue = !memory.valuesIterator.forall(identity)
      if (newValue) {
        Tracker.bump(name)
      }
      signals += newValue
    }
    override def addSource(from: String): Unit = memory(from) = false
    def current = !memory.valuesIterator.forall(identity)
  }

  case class Broadcaster(override val targets: Seq[String]) extends Module(targets) {
    var state = false
    def process(from: String, value: Boolean) = {
      state = value
      signals += value
    }
    def current = state
  }

  case object Button extends Module(List("broadcaster")) {
    def process(from: String, value: Boolean): Unit = {}
    def current = false
  }

  val inputx = """broadcaster -> a, b, c
                |%a -> b
                |%b -> c
                |%c -> inv
                |&inv -> a""".stripMargin.split("\n")

  val input2 = """broadcaster -> a
                |%a -> inv, con
                |&inv -> b
                |%b -> con
                |&con -> output""".stripMargin.split("\n")

  val input = Source.fromFile("data2023/20").mkString.split("\n")
  println("Size", input.size)

  val modules: Map[String, Module] = input.map(_.split(" -> ")).map{ case Array(mod, targetString) =>
    val targets = targetString.split(", ")
    mod match {
      case "broadcaster" => (mod, Broadcaster(targets))
      case name if name.startsWith("%") => (name.substring(1), FlipFlop(targets))
      case name if name.startsWith("&") => (name.substring(1), Conjunction(name.substring(1), targets))
    }
  }.toMap

  modules.foreach{ case (name, mod) =>
    mod.targets.foreach(modules.get(_).map(_.addSource(name)))
    mod.targets.foreach{target => if (!modules.contains(target)) println(target)}
  }

  case object Tracker {
    var nr = 0
    val interesting = List("zl", "xn", "qn", "xf")
    val counters = interesting
      .map(v => v -> scala.collection.mutable.MutableList[Int]()).toMap

    var lows = 0
    var highs = 0
    def sendTo(from: String, targets: Seq[String], value: Boolean) = {
      //println(s"$from -${if (value) "high" else "low"}-> ${targets.mkString(", ")}")
      if (value)
        highs += targets.length
      else
        lows += targets.length
      if (targets.contains("rx") && !value)
        println("rx", "tjoho")
      targets.foreach(modules.get(_).map(_.process(from, value)))
    }
    def bump(name: String) = {
      if (interesting.contains(name))
        counters(name) += nr
    }
  }


  def run(nr: Int) = {
    //println("===")
    Tracker.sendTo("button", List("broadcaster"), false)
    Tracker.nr = nr
    var triggered = modules.filter(_._2.signals.nonEmpty)
    while (triggered.nonEmpty) {
      //println("---")
      val signals = triggered.flatMap{case (name, mod)=> mod.get.map((name, mod.targets, _))}
      signals.foreach{ case (from, targets, value) => Tracker.sendTo(from, targets, value)}
      triggered = modules.filter(_._2.signals.nonEmpty)
    }
  }

  lazy val part1 = {
    (1 to 100000).foreach(run(_))
    println(Tracker.lows)
    println(Tracker.highs)
    println(BigInt(Tracker.lows) * Tracker.highs)
    Tracker.counters.foreach { case (v, counter) =>
      println(s"$v = ${counter.take(3)}  ${counter.toList.sliding(2).map{ case a::b::Nil => b-a}.toList}")
    }
  }
}

object Day20Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day20.part1)
    println(LocalDateTime.now())
  }
}
