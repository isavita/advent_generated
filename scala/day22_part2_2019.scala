
import scala.io.Source
import scala.math.BigInt

val Size = BigInt("119315717514047")

case class Deck(var step: BigInt, var direction: BigInt, var top: BigInt) {
  def dealIntoNewStack(): Unit = {
    top = (top - direction * step + Size) % Size
    direction *= -1
  }

  def cutN(n: BigInt): Unit = {
    top = (top + direction * step * n + Size) % Size
  }

  def dealWithIncrementN(n: BigInt): Unit = {
    val inv = modinv(n, Size)
    step *= inv
    top *= inv
  }

  def pick(n: BigInt): BigInt = {
    var current = top
    for (_ <- 0 until n.toInt) {
      current = ((current + direction * step) % Size + Size) % Size
    }
    current
  }
}

def modinv(a: BigInt, m: BigInt): BigInt = {
  val (_, x, _) = egcd(a, m)
  val result = if (x < 0) x + m else x
  result % m
}

def egcd(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
  if (a == 0) {
    (b, 0, 1)
  } else {
    val (gcd, y, x) = egcd(b % a, a)
    (gcd, x - (b / a) * y, y)
  }
}

def main(args: Array[String]): Unit = {
  val file = Source.fromFile("input.txt")
  var offset = BigInt(0)
  var increment = BigInt(1)

  for (line <- file.getLines) {
    if (line == "deal into new stack") {
      increment *= -1
      offset += increment
    } else if (line.startsWith("cut")) {
      val n = line.split(" ")(1).toInt
      offset += BigInt(n) * increment
    } else if (line.startsWith("deal with increment")) {
      val n = line.split(" ").last.toInt
      increment *= BigInt(n).modPow(Size - 2, Size)
    }
  }

  val iter = BigInt("101741582076661")
  val finalIncr = increment.modPow(iter, Size)
  val finalOffs = (BigInt(1) - finalIncr) * modinv(increment - BigInt(1), Size) * offset
  val answer = (BigInt(2020) * finalIncr + finalOffs) % Size

  println(answer)
}
