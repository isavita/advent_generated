
import scala.io.Source
import scala.collection.mutable

object HappinessCalculator {
  def main(args: Array[String]): Unit = {
    val happinessMap = readHappinessValues("input.txt")
    addYourself(happinessMap)
    val guests = happinessMap.keys.toArray
    val maxHappiness = calculateOptimalArrangement(guests, happinessMap)
    println(maxHappiness)
  }

  def readHappinessValues(filename: String): mutable.Map[String, mutable.Map[String, Int]] = {
    val happinessMap = mutable.Map[String, mutable.Map[String, Int]]()
    for (line <- Source.fromFile(filename).getLines()) {
      val parts = line.split(" ")
      if (parts.length >= 11) {
        val from = parts(0)
        val to = parts(10).dropRight(1)
        val change = if (parts(2) == "lose") -parts(3).toInt else parts(3).toInt
        happinessMap.getOrElseUpdate(from, mutable.Map[String, Int]())(to) = change
      }
    }
    happinessMap
  }

  def addYourself(happinessMap: mutable.Map[String, mutable.Map[String, Int]]): Unit = {
    happinessMap("You") = mutable.Map[String, Int]()
    for (guest <- happinessMap.keys) {
      happinessMap(guest)("You") = 0
      happinessMap("You")(guest) = 0
    }
  }

  def calculateOptimalArrangement(guests: Array[String], happinessMap: mutable.Map[String, mutable.Map[String, Int]]): Int = {
    guests.permutations.map(arrangement => calculateHappiness(arrangement.toArray, happinessMap)).max
  }

  def calculateHappiness(arrangement: Array[String], happinessMap: mutable.Map[String, mutable.Map[String, Int]]): Int = {
    arrangement.indices.map { i =>
      val left = arrangement((i + arrangement.length - 1) % arrangement.length)
      val right = arrangement((i + 1) % arrangement.length)
      happinessMap(arrangement(i))(left) + happinessMap(arrangement(i))(right)
    }.sum
  }
}
