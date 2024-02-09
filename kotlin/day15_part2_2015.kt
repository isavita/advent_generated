import java.io.File

data class Ingredient(val name: String, val capacity: Int, val durability: Int, val flavor: Int, val texture: Int, val calories: Int)

fun main() {
    val ingredients = mutableListOf<Ingredient>()

    File("input.txt").readLines().forEach {
        val parts = it.split(" ")
        val name = parts[0].dropLast(1)
        val capacity = parts[2].dropLast(1).toInt()
        val durability = parts[4].dropLast(1).toInt()
        val flavor = parts[6].dropLast(1).toInt()
        val texture = parts[8].dropLast(1).toInt()
        val calories = parts[10].toInt()
        ingredients.add(Ingredient(name, capacity, durability, flavor, texture, calories))
    }

    var maxScore = 0
    var maxScore500Calories = 0

    for (a in 0..100) {
        for (b in 0..100) {
            for (c in 0..100) {
                for (d in 0..100) {
                    if (a + b + c + d == 100) {
                        val capacity = ingredients[0].capacity * a + ingredients[1].capacity * b + ingredients[2].capacity * c + ingredients[3].capacity * d
                        val durability = ingredients[0].durability * a + ingredients[1].durability * b + ingredients[2].durability * c + ingredients[3].durability * d
                        val flavor = ingredients[0].flavor * a + ingredients[1].flavor * b + ingredients[2].flavor * c + ingredients[3].flavor * d
                        val texture = ingredients[0].texture * a + ingredients[1].texture * b + ingredients[2].texture * c + ingredients[3].texture * d
                        val calories = ingredients[0].calories * a + ingredients[1].calories * b + ingredients[2].calories * c + ingredients[3].calories * d

                        val score = if (capacity < 0 || durability < 0 || flavor < 0 || texture < 0) {
                            0
                        } else {
                            capacity * durability * flavor * texture
                        }

                        if (score > maxScore) {
                            maxScore = score
                        }

                        if (calories == 500 && score > maxScore500Calories) {
                            maxScore500Calories = score
                        }
                    }
                }
            }
        }
    }

    println(maxScore)
    println(maxScore500Calories)
}