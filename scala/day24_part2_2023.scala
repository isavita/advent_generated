
import scala.io.Source

case class RatVec3(X: BigInt, Y: BigInt, Z: BigInt) {
  def add(other: RatVec3): RatVec3 = RatVec3(X + other.X, Y + other.Y, Z + other.Z)
  def subtract(other: RatVec3): RatVec3 = RatVec3(X - other.X, Y - other.Y, Z - other.Z)
  def multiply(s: BigInt): RatVec3 = RatVec3(X * s, Y * s, Z * s)
  def divide(s: BigInt): RatVec3 = RatVec3(X / s, Y / s, Z / s)
  def cross(other: RatVec3): RatVec3 = RatVec3(Y * other.Z - Z * other.Y, Z * other.X - X * other.Z, X * other.Y - Y * other.X)
  def dot(other: RatVec3): BigInt = X * other.X + Y * other.Y + Z * other.Z
}

case class HailStone(p: RatVec3, v: RatVec3) {
  def subtract(other: HailStone): HailStone = HailStone(p.subtract(other.p), v.subtract(other.v))
}

object Solution {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    println(solve(input))
  }

  def solve(input: List[String]): String = {
    val hailStones = readInput(input.take(3))
    val s1 = hailStones(1)
    val s2 = hailStones(2)
    val ref1 = s1.subtract(hailStones(0))
    val ref2 = s2.subtract(hailStones(0))

    val t1 = intersectionTime(ref2, ref1)
    val t2 = intersectionTime(ref1, ref2)

    val rock1 = s1.p.add(s1.v.multiply(t1))
    val rock2 = s2.p.add(s2.v.multiply(t2))

    val rp = rock1.subtract(rock2.subtract(rock1).divide(t2 - t1).multiply(t1))
    (rp.X + rp.Y + rp.Z).toString
  }

  def readInput(input: List[String]): List[HailStone] = {
    input.map(readLine)
  }

  def readLine(line: String): HailStone = {
    val m = "-?\\d+".r.findAllIn(line).map(BigInt(_)).toList
    HailStone(
      RatVec3(m(0), m(1), m(2)),
      RatVec3(m(3), m(4), m(5))
    )
  }

  def intersectionTime(r: HailStone, s: HailStone): BigInt = {
    val plane = r.p.cross(r.p.add(r.v))
    -s.p.dot(plane) / s.v.dot(plane)
  }
}
