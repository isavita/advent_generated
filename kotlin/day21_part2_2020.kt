import java.io.File

data class Food(val ingredients: List<String>, val allergens: List<String>)

fun main() {
    val input = File("input.txt").readLines()
    val foods = input.map { line ->
        val ingredientsPart = line.substringBefore(" (contains ")
        val allergensPart = line.substringAfter(" (contains ").removeSuffix(")")
        val ingredients = ingredientsPart.split(" ")
        val allergens = allergensPart.split(", ")
        Food(ingredients, allergens)
    }

    // Part 1: Determine which ingredients can't possibly contain any allergens
    val allIngredients = mutableSetOf<String>()
    val allergenToIngredients = mutableMapOf<String, MutableSet<String>>()

    foods.forEach { food ->
        allIngredients.addAll(food.ingredients)
        food.allergens.forEach { allergen ->
            if (!allergenToIngredients.containsKey(allergen)) {
                allergenToIngredients[allergen] = food.ingredients.toMutableSet()
            } else {
                allergenToIngredients[allergen]?.retainAll(food.ingredients)
            }
        }
    }

    val ingredientsWithoutAllergens = allIngredients.filter { ingredient ->
        allergenToIngredients.values.none { it.contains(ingredient) }
    }

    val count = foods.flatMap { it.ingredients }.count { it in ingredientsWithoutAllergens }
    println(count) // Output for part 1

    // Part 2: Determine the canonical dangerous ingredient list
    val allergenToIngredient = mutableMapOf<String, String>()
    while (allergenToIngredients.isNotEmpty()) {
        val (allergen, ingredients) = allergenToIngredients.entries.find { it.value.size == 1 }!!
        val ingredient = ingredients.first()
        allergenToIngredient[allergen] = ingredient
        allergenToIngredients.remove(allergen)
        allergenToIngredients.values.forEach { it.remove(ingredient) }
    }

    val canonicalDangerousIngredientList = allergenToIngredient.entries
        .sortedBy { it.key }
        .joinToString(",") { it.value }

    println(canonicalDangerousIngredientList) // Output for part 2
}