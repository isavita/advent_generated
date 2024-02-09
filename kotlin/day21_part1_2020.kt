import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val allergenMap = mutableMapOf<String, MutableMap<String, Boolean>>()
    val ingredientCount = mutableMapOf<String, Int>()
    val safeIngredients = mutableMapOf<String, Boolean>()

    file.forEachLine {
        val parts = it.split(" (contains ")
        val ingredients = parts[0].split(" ")
        val allergens = if (parts.size > 1) parts[1].substring(0, parts[1].length - 1).split(", ") else emptyList()

        ingredients.forEach { ingredient ->
            ingredientCount[ingredient] = ingredientCount.getOrDefault(ingredient, 0) + 1
            safeIngredients[ingredient] = true
        }

        allergens.forEach { allergen ->
            if (!allergenMap.containsKey(allergen)) {
                allergenMap[allergen] = mutableMapOf()
                ingredients.forEach { ingredient ->
                    allergenMap[allergen]!![ingredient] = true
                }
            } else {
                allergenMap[allergen]!!.keys.retainAll(ingredients)
            }
        }
    }

    allergenMap.values.forEach { ingredients ->
        ingredients.keys.forEach { ingredient ->
            safeIngredients.remove(ingredient)
        }
    }

    var count = 0
    safeIngredients.keys.forEach { ingredient ->
        count += ingredientCount[ingredient]!!
    }

    println(count)
}