
import scala.io.Source

sealed trait SnailNumber {
  def magnitude: Int

  def add(other: SnailNumber): SnailNumber = {
    SnailPair(this, other).reduce()
  }

  def reduce(): SnailNumber = {
    var current = this
    while (true) {
      val (exploded, _, _, newNumber) = current.explode(0)
      if (exploded) {
        current = newNumber
      } else {
        val (split, newNumber2) = current.split()
        if (split) {
          current = newNumber2
        } else {
          return current
        }
      }
    }
    current
  }

  def explode(depth: Int): (Boolean, Int, Int, SnailNumber)

  def split(): (Boolean, SnailNumber)

  def addLeft(value: Int): SnailNumber

  def addRight(value: Int): SnailNumber
}

case class SnailValue(var value: Int) extends SnailNumber {
  override def magnitude: Int = value

  override def explode(depth: Int): (Boolean, Int, Int, SnailNumber) = (false, 0, 0, this)

  override def split(): (Boolean, SnailNumber) = {
    if (value >= 10) {
      (true, SnailPair(SnailValue(value / 2), SnailValue((value + 1) / 2)))
    } else {
      (false, this)
    }
  }

  override def addLeft(value: Int): SnailNumber = {
    this.value += value
    this
  }

  override def addRight(value: Int): SnailNumber = {
    this.value += value
    this
  }
}

case class SnailPair(var left: SnailNumber, var right: SnailNumber) extends SnailNumber {
  override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude

  override def explode(depth: Int): (Boolean, Int, Int, SnailNumber) = {
    if (depth == 4) {
      (true, left.asInstanceOf[SnailValue].value, right.asInstanceOf[SnailValue].value, SnailValue(0))
    } else {
      val (explodedLeft, leftValue, rightValue, newLeft) = left.explode(depth + 1)
      if (explodedLeft) {
        if (rightValue > 0) {
          (true, leftValue, 0, SnailPair(newLeft, right.addLeft(rightValue)))
        } else {
          (true, leftValue, 0, SnailPair(newLeft, right))
        }
      } else {
        val (explodedRight, leftValue2, rightValue2, newRight) = right.explode(depth + 1)
        if (explodedRight) {
          if (leftValue2 > 0) {
            (true, 0, rightValue2, SnailPair(left.addRight(leftValue2), newRight))
          } else {
            (true, 0, rightValue2, SnailPair(left, newRight))
          }
        } else {
          (false, 0, 0, this)
        }
      }
    }
  }

  override def split(): (Boolean, SnailNumber) = {
    val (splitLeft, newLeft) = left.split()
    if (splitLeft) {
      (true, SnailPair(newLeft, right))
    } else {
      val (splitRight, newRight) = right.split()
      if (splitRight) {
        (true, SnailPair(left, newRight))
      } else {
        (false, this)
      }
    }
  }

  override def addLeft(value: Int): SnailNumber = {
    left = left.addLeft(value)
    this
  }

  override def addRight(value: Int): SnailNumber = {
    right = right.addRight(value)
    this
  }
}

object Snailfish {
  def parseSnailNumber(input: String): SnailNumber = {
    if (input(0) != '[') {
      SnailValue(input.toInt)
    } else {
      var balance = 0
      var splitIndex = 0
      for (i <- 1 until input.length - 1) {
        input(i) match {
          case '[' => balance += 1
          case ']' => balance -= 1
          case ',' =>
            if (balance == 0) {
              splitIndex = i
            }
          case _ =>
        }
        if (splitIndex != 0) {
          return SnailPair(parseSnailNumber(input.substring(1, splitIndex)), parseSnailNumber(input.substring(splitIndex + 1, input.length - 1)))
        }
      }
      throw new IllegalArgumentException("Invalid input")
    }
  }

  def deepCopy(sn: SnailNumber): SnailNumber = {
    sn match {
      case SnailValue(value) => SnailValue(value)
      case SnailPair(left, right) => SnailPair(deepCopy(left), deepCopy(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val snailNumbers = Source.fromFile("input.txt").getLines().map(parseSnailNumber).toList
    var largestMagnitude = 0
    for (i <- snailNumbers.indices) {
      for (j <- snailNumbers.indices) {
        if (i != j) {
          val sum1 = deepCopy(snailNumbers(i)).add(deepCopy(snailNumbers(j))).magnitude
          val sum2 = deepCopy(snailNumbers(j)).add(deepCopy(snailNumbers(i))).magnitude
          largestMagnitude = largestMagnitude.max(sum1).max(sum2)
        }
      }
    }
    println(largestMagnitude)
  }
}
