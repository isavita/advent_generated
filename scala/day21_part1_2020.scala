
object AllergenAssessment extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val foodList = input.map { line =>
    val Array(ingredients, allergens) = line.dropRight(1).split(" \\(contains ")
    (ingredients.split(" ").toSet, allergens.split(", "))
  }

  val allIngredients = foodList.flatMap(_._1).toSet
  val allAllergens = foodList.flatMap(_._2).toSet

  val possibleIngredients = allAllergens.map { allergen =>
    val foodsWithAllergen = foodList.filter(_._2.contains(allergen)).map(_._1)
    allergen -> foodsWithAllergen.reduce(_.intersect(_))
  }.toMap

  val ingredientsWithAllergens = possibleIngredients.values.flatten.toSet
  val safeIngredients = allIngredients.diff(ingredientsWithAllergens)

  val answer = foodList.map(_._1.count(safeIngredients.contains)).sum
  println(answer)
}
