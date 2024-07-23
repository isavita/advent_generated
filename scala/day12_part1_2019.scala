
import scala.io.Source

case class Vec3(x: Int, y: Int, z: Int)
case class Moon(pos: Vec3, vel: Vec3)

object MoonSimulation {
  def main(args: Array[String]): Unit = {
    val moons = Source.fromFile("input.txt").getLines().toList.map {
      case s"<x=$x, y=$y, z=$z>" => Moon(Vec3(x.toInt, y.toInt, z.toInt), Vec3(0, 0, 0))
    }

    val finalMoons = (0 until 1000).foldLeft(moons) { (moons, _) =>
      applyGravity(moons).map(applyVelocity)
    }

    println(totalEnergy(finalMoons))
  }

  def applyGravity(moons: List[Moon]): List[Moon] = {
    moons.indices.foldLeft(moons) { (ms, i) =>
      moons.indices.foldLeft(ms) { (mss, j) =>
        if (i < j) {
          val (m1, m2) = (mss(i), mss(j))
          val newVel1 = m1.vel.copy(
            x = m1.vel.x + sign(m2.pos.x - m1.pos.x),
            y = m1.vel.y + sign(m2.pos.y - m1.pos.y),
            z = m1.vel.z + sign(m2.pos.z - m1.pos.z)
          )
          val newVel2 = m2.vel.copy(
            x = m2.vel.x + sign(m1.pos.x - m2.pos.x),
            y = m2.vel.y + sign(m1.pos.y - m2.pos.y),
            z = m2.vel.z + sign(m1.pos.z - m2.pos.z)
          )
          mss.updated(i, m1.copy(vel = newVel1)).updated(j, m2.copy(vel = newVel2))
        } else mss
      }
    }
  }

  def applyVelocity(moon: Moon): Moon = {
    moon.copy(pos = Vec3(
      moon.pos.x + moon.vel.x,
      moon.pos.y + moon.vel.y,
      moon.pos.z + moon.vel.z
    ))
  }

  def totalEnergy(moons: List[Moon]): Int = {
    moons.map { m =>
      val pot = math.abs(m.pos.x) + math.abs(m.pos.y) + math.abs(m.pos.z)
      val kin = math.abs(m.vel.x) + math.abs(m.vel.y) + math.abs(m.vel.z)
      pot * kin
    }.sum
  }

  def sign(x: Int): Int = if (x > 0) 1 else if (x < 0) -1 else 0
}
