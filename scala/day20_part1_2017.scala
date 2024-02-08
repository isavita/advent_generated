
object ParticleSwarm extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  case class Particle(id: Int, p: (Int, Int, Int), v: (Int, Int, Int), a: (Int, Int, Int)) {
    def distance: Int = p._1.abs + p._2.abs + p._3.abs
  }

  def parseInput(input: List[String]): List[Particle] = {
    input.zipWithIndex.map {
      case (line, index) =>
        val data = line.split(", ").map(_.drop(3).dropRight(1).split(",").map(_.toInt))
        Particle(index, (data(0)(0), data(0)(1), data(0)(2)), (data(1)(0), data(1)(1), data(1)(2)), (data(2)(0), data(2)(1), data(2)(2)))
    }
  }

  def updateParticle(particle: Particle): Particle = {
    val newV = (particle.v._1 + particle.a._1, particle.v._2 + particle.a._2, particle.v._3 + particle.a._3)
    val newP = (particle.p._1 + newV._1, particle.p._2 + newV._2, particle.p._3 + newV._3)
    Particle(particle.id, newP, newV, particle.a)
  }

  def findClosestParticle(input: List[Particle]): Int = {
    val updatedParticles = (1 to 1000).foldLeft(input) { (particles, _) =>
      particles.map(updateParticle)
    }
    updatedParticles.minBy(_.distance).id
  }

  val particles = parseInput(input)
  println(findClosestParticle(particles))
}
