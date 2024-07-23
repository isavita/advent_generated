
import scala.io.Source
import scala.collection.mutable

case class Chemical(name: String, amount: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val (reactions, ingredients) = parseInput("input.txt")
    println(calculateOre("FUEL", 1, reactions, ingredients, mutable.Map.empty))
  }

  def parseInput(filename: String): (Map[String, Chemical], Map[String, List[Chemical]]) = {
    val lines = Source.fromFile(filename).getLines().toList
    val reactions = mutable.Map[String, Chemical]()
    val ingredients = mutable.Map[String, List[Chemical]]()

    for (line <- lines) {
      val parts = line.split(" => ")
      val output = parseChemical(parts(1))
      val inputs = parts(0).split(", ").toList.map(parseChemical)
      reactions(output.name) = output
      ingredients(output.name) = inputs
    }

    (reactions.toMap, ingredients.toMap)
  }

  def parseChemical(s: String): Chemical = {
    val parts = s.split(" ")
    Chemical(parts(1), parts(0).toInt)
  }

  def calculateOre(chem: String, amount: Int, reactions: Map[String, Chemical], ingredients: Map[String, List[Chemical]], surplus: mutable.Map[String, Int]): Int = {
    if (chem == "ORE") return amount

    val available = surplus.getOrElse(chem, 0)
    if (available >= amount) {
      surplus(chem) = available - amount
      return 0
    }

    val required = amount - available
    surplus(chem) = 0
    val reaction = reactions(chem)
    val times = (required + reaction.amount - 1) / reaction.amount
    var ore = 0

    for (ingredient <- ingredients(chem)) {
      ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    }

    surplus(chem) = times * reaction.amount - required
    ore
  }
}
