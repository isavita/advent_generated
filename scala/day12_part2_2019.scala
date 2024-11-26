
import scala.io.Source
import scala.math.BigInt

case class Vec3(x: Int, y: Int, z: Int)
case class Moon(pos: Vec3, vel: Vec3)

object Solution extends App {
  val source = Source.fromFile("input.txt")
  val lines = source.getLines().toList
  source.close()

  val initialMoons = lines.map { line =>
    val Array(x, y, z) = """<x=(.+), y=(.+), z=(.+)>""".r.findAllIn(line).matchData.next().subgroups.map(_.toInt).toArray
    Moon(Vec3(x, y, z), Vec3(0, 0, 0))
  }

  def applyGravity(moons: List[Moon], axis: Char): List[Moon] = {
    moons.zipWithIndex.map { case (m1, i) =>
      val velChange = moons.zipWithIndex.filter(_._1 != m1).map { case (m2, j) =>
        axis match {
          case 'x' => if (m1.pos.x > m2.pos.x) (-1, 0, 0) else if (m1.pos.x < m2.pos.x) (1, 0, 0) else (0, 0, 0)
          case 'y' => if (m1.pos.y > m2.pos.y) (0, -1, 0) else if (m1.pos.y < m2.pos.y) (0, 1, 0) else (0, 0, 0)
          case 'z' => if (m1.pos.z > m2.pos.z) (0, 0, -1) else if (m1.pos.z < m2.pos.z) (0, 0, 1) else (0, 0, 0)
        }
      }.reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))
      m1.copy(vel = Vec3(m1.vel.x + velChange._1, m1.vel.y + velChange._2, m1.vel.z + velChange._3))
    }
  }

  def applyVelocity(moons: List[Moon], axis: Char): List[Moon] = {
    moons.map { m =>
      axis match {
        case 'x' => m.copy(pos = Vec3(m.pos.x + m.vel.x, m.pos.y, m.pos.z))
        case 'y' => m.copy(pos = Vec3(m.pos.x, m.pos.y + m.vel.y, m.pos.z))
        case 'z' => m.copy(pos = Vec3(m.pos.x, m.pos.y, m.pos.z + m.vel.z))
      }
    }
  }

  def findCycle(moons: List[Moon], initialMoons: List[Moon], axis: Char): Long = {
    var steps = 0L
    var currentMoons = moons
    while (true) {
      steps += 1
      currentMoons = applyVelocity(applyGravity(currentMoons, axis), axis)
      if (currentMoons.zip(initialMoons).forall { case (m1, m2) =>
        axis match {
          case 'x' => m1.pos.x == m2.pos.x && m1.vel.x == m2.vel.x
          case 'y' => m1.pos.y == m2.pos.y && m1.vel.y == m2.vel.y
          case 'z' => m1.pos.z == m2.pos.z && m1.vel.z == m2.vel.z
        }
      }) return steps
    }
    0L //unreachable
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
  def lcm(a: BigInt, b: BigInt): BigInt = (a * b) / gcd(a, b)

  val cycleX = findCycle(initialMoons, initialMoons, 'x')
  val cycleY = findCycle(initialMoons, initialMoons, 'y')
  val cycleZ = findCycle(initialMoons, initialMoons, 'z')

  println(lcm(lcm(BigInt(cycleX), BigInt(cycleY)), BigInt(cycleZ)))
}
