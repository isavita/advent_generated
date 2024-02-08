
object Solution extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  case class Particle(p: (Int, Int, Int), v: (Int, Int, Int), a: (Int, Int, Int)) {
    def update(): Particle = {
      val newV = (v._1 + a._1, v._2 + a._2, v._3 + a._3)
      val newP = (p._1 + newV._1, p._2 + newV._2, p._3 + newV._3)
      Particle(newP, newV, a)
    }

    def distanceFromOrigin(): Int = p._1.abs + p._2.abs + p._3.abs
  }

  val particles = input.zipWithIndex.map {
    case (line, index) =>
      val parts = line.split(", ").map(_.drop(3).dropRight(1).split(",").map(_.toInt))
      Particle((parts(0)(0), parts(0)(1), parts(0)(2)), (parts(1)(0), parts(1)(1), parts(1)(2)), (parts(2)(0), parts(2)(1), parts(2)(2)))
  }

  def removeCollisions(particles: List[Particle]): List[Particle] = {
    particles.groupBy(_.p).values.filter(_.size == 1).flatten.toList
  }

  def simulate(particles: List[Particle], ticks: Int): Int = {
    if (ticks == 0) particles.length
    else {
      val updatedParticles = particles.map(_.update())
      simulate(removeCollisions(updatedParticles), ticks - 1)
    }
  }

  // Part One
  println(particles.zipWithIndex.minBy(p => (p._1.distanceFromOrigin(), p._2))._2)

  // Part Two
  println(simulate(particles, 1000))
}
