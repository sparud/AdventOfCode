package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day24 {
  val input = Source.fromFile("data2023/24").mkString.split("\n")

  case class Path(x: Double, y: Double, z: Double, dx: Double, dy: Double, dz: Double) {
    self =>
    def intersection(o: Path, min: Double, max: Double) = {
      val t1 = (o.y - y + o.dy / o.dx * (x - o.x)) / (dy - o.dy * dx / o.dx)
      val denom = dy - o.dy * dx / o.dx
      if (denom == 0)
        println("0!", self, o)
      val t2 = (x + dx * t1 - o.x) / o.dx
      val ix = x + t1 * dx
      val iy = y + t1 * dy
      if (t1 >= 0 && t2 >= 0 && ix >= min && ix <= max && iy >= min && iy <= max) {
        Some((ix, iy))
      } else
        None
    }
  }

  val paths = input.map(line => line.split("[, @]+").map(_.toDouble) match {
    case Array(x, y, z, dx, dy, dz) => Path(x, y, z, dx, dy, dz)
  }).toList

  @tailrec
  def check(ps: List[Path], acc: List[(Double, Double)] = Nil): List[(Double, Double)] = ps match {
    case p :: Nil => acc
    case p :: rest => check(rest, rest.flatMap(_.intersection(p, 2e14, 4e14)) ++ acc)
    //case p::rest => check(rest, rest.flatMap(o => p.intersection(o, 7, 27)) ++ acc)
   }

  lazy val part1 = check(paths).length

  case class Path2(x: BigInt, y: BigInt, z: BigInt, dx: BigInt, dy: BigInt, dz: BigInt) {
    def between(min: BigInt, max: BigInt, v: BigInt) = v >= min && v <= max

    def findPositions(o: Path2, oo: Path2, min: BigInt, max: BigInt) =
      (-1000 to 1000).flatMap(vx =>
        (-1000 to 1000).flatMap { vy =>
          val t2numerator = -vx * y + vx * o.y + dx * y - dx * o.y + vy * x - vy * o.x - dy * x + dy * o.x
          val t2denominator = vx * dy - vx * o.dy - dx * vy + dx * o.dy + o.dx * vy - o.dx * dy
          if (t2denominator != 0 && t2numerator % t2denominator == 0) {
            val t2 = t2numerator / t2denominator
            val t1numerator = x - y - o.x + o.y + t2 * (vx - vy - o.dx + o.dy)
            val t1denominator = vx - vy - dx + dy
            if (t2 >=0 && t1denominator != 0 && t1numerator % t1denominator == 0) {
              val t1 = t1numerator / t1denominator
              val px = x + -t1 * vx + t1 * dx
              val py = y + -t1 * vy + t1 * dy
              val vznumerator = z - o.z + t1 * dz - t2 * o.dz
              val vzdenominator = t1 - t2
              if (t1 >= 0 && vzdenominator != 0 && vznumerator % vzdenominator == 0) {
                val vz = vznumerator/vzdenominator
                val pz = z + t1 * dz - t1 * vz
                val t3numerator = oo.x-px
                val t3denomitator = vx-oo.dx
                if (t3denomitator != 0 && t3numerator % t3denomitator == 0) {
                  val t3 = t3numerator / t3denomitator
                  Some(px+py+pz)
                } else
                 None
              } else
                None
            } else None
          } else None
        }
      ).head
  }

  val paths2 = input.map(line => line.split("[, @]+").map(BigInt(_)) match {
    case Array(x, y, z, dx, dy, dz) => Path2(x, y, z, dx, dy, dz)
  }).toList

  lazy val part2 = paths2.head.findPositions(paths2.tail.head, paths2.tail.tail.head, BigInt("200000000000000"), BigInt("400000000000000"))
}

object Day24Runner {
  """
    |x + t1*dx = x1 + t1*dx1  => t1*dx - t2*dx = x1 + t1*dx1 - x2 - t2*dx2
    |y + t1*dy = y1 + t1*dy1  => t1*dy - t2*dy = y1 + t1*dy1 - y2 - t2*dy2
    |z + t1*dz = z1 + t1*dz1  => t1*(dx-dy) - t2*(dx-dy) = x1 - y1 -x2 + y2 + t1*(dx1-dy1) - t2*(dx2-dy2)
    |x + t2*dx = x2 + t2*dx2  => t1*(dx-dy) - t1*(dx1-dy1) = x1 - y1 -x2 + y2 + t2*(dx-dy) - t2*(dx2-dy2)
    |y + t2*dy = y2 + t2*dy2  -- t1*(dx-dy-dx1+dy1) = x1 - y1 -x2 + y2 + t2*(dx-dy-dx2+dy2)
    |z + t2*dz = z2 + t2*dz2  == t1 = (x1 - y1 - x2 + y2 + t2*(dx-dy-dx2+dy2)) / (dx-dy-dx1+dy1)
    |x + t3*dx = x3 + t3*dx3  == t3 = (x3-x)/(dx-dx3)
    |y + t3*dy = y3 + t3*dy3
    |z + t3*dz = z3 + t3*dz3  -> t1*(dx-dx1) = x1-x2 + t2*(dx-dx2)
    |                         -> t1 = (x1-x2 + t2*(dx-dx2))/(dx-dx1)
    |
    |  -> (x1-x2 + t2*(dx-dx2))/(dx-dx1)*dx -t2*dx = x1 -x2 + (x1-x2 + t2*(dx-dx2))/(dx-dx1)*dx1 - t2*dx2
    |  -> dx*(x1-x2 + t2*(dx-dx2)) - t2*dx*(dx-dx1) = (x1-x2)*(dx-dx1) + (x1-x2 + t2*(dx-dx2))*dx2 -t2*dx2*(dx-dx1)
    |  -> dx*(x1-x2) + t2*dx*(dx-x2) - t2*dx*(dx-dx1) = (x1-x2)*(dx-dx1) + dx2*(x1-x2) + t2*dx2*(dx-dx2) -t2*dx2*(dx-dx1)
    |  -> dx*(x1-x2) + t2*dx*(dx1-dx2) = (x1-x2)*(dx-dx1) + dx2*(x1-x2) + t2*dx2*(dx1-dx2)
    |  -> t2*dx*(dx1-dx2) - t2*dx2*(dx1-dx2) = (x1-x2)*(dx-dx1) + dx2*(x1-x2) - dx*(x1-x2)
    |  -> t2*(dx-dx2)*(dx1-dx2) = (x1-x2)*(dx-dx1+dx2-dx) = (x1-x2)*(dx2-dx1)
    |  -> t2 = (x1-x2)*(dx2-dx1) / (dx-dx2) / (dx1-dx2) = -(x1-x2)/(dx-dx2) = (x2-x1)/(dx-dx2)
    |  ...
    |  -> t2 = (y2-y1)/(dy-dy2)
    |
    |   (x2-x1)/(dx-dx2) = (y2-y1)/(dy-dy2)
    |
    |   (x2-x1)*(dy-dy2) = (y2-y1)*(dx-dx2)
    |
    |
    | z + t1*dz = z1 + t1*dz1
    | z + t2*dz = z2 + t2*dz2
    |
    | dz*(t1-t2) = z1 - z2 + t1*dz1 - t2*dz2
    |
    | vz = (z - o.z + t1*dz - t2*o.dz) / (t1 - t2)
    |
    |""".stripMargin
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day24.part2)
    println(LocalDateTime.now())
  }
}
