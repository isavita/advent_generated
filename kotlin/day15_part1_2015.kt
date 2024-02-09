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
    for (i in 0..100) {
        for (j in 0..100 - i) {
            for (k in 0..100 - i - j) {
                val l = 100 - i - j - k

                val capacity = maxOf(0, i * ingredients[0].capacity + j * ingredients[1].capacity + k * ingredients[2].capacity + l * ingredients[3].capacity)
                val durability = maxOf(0, i * ingredients[0].durability + j * ingredients[1].durability + k * ingredients[2].durability + l * ingredients[3].durability)
                val flavor = maxOf(0, i * ingredients[0].flavor + j * ingredients[1].flavor + k * ingredients[2].flavor + l * ingredients[3].flavor)
                val texture = maxOf(0, i * ingredients[0].texture + j * ingredients[1].texture + k * ingredients[2].texture + l * ingredients[3].texture)

                val score = capacity * durability * flavor * texture
                maxScore = maxOf(maxScore, score)
            }
        }
    }

    println(maxScore)
}