
import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  val file = Source.fromFile(filename)
  val allergenMap = collection.mutable.Map[String, Map[String, Boolean]]()
  val ingredientAllergen = collection.mutable.Map[String, String]()

  for (line <- file.getLines) {
    val parts = line.split(" \\(contains ")
    val ingredients = parts(0).split(" ")
    val allergens = if (parts.length > 1) parts(1).dropRight(1).split(", ") else Array[String]()

    for (allergen <- allergens) {
      if (!allergenMap.contains(allergen)) {
        allergenMap(allergen) = ingredients.map(_ -> true).toMap
      } else {
        for ((ingredient, _) <- allergenMap(allergen)) {
          if (!ingredients.contains(ingredient)) {
            allergenMap(allergen) -= ingredient
          }
        }
      }
    }
  }

  while (allergenMap.nonEmpty) {
    for ((allergen, ingredients) <- allergenMap) {
      if (ingredients.size == 1) {
        val ingredient = ingredients.head._1
        ingredientAllergen(allergen) = ingredient
        allergenMap.keys.foreach(key => allergenMap(key) -= ingredient)
        allergenMap -= allergen
      }
    }
  }

  val allergens = ingredientAllergen.keys.toList.sorted
  val result = allergens.map(allergen => ingredientAllergen(allergen))

  println(result.mkString(","))
  file.close()
}
