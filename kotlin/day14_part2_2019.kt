
import java.io.File
import kotlin.math.max

data class Chemical(val name: String, val amount: Int)

fun parseChemical(s: String): Chemical {
    val parts = s.split(" ")
    return Chemical(parts[1], parts[0].toInt())
}

fun main() {
    val reactions = mutableMapOf<String, Chemical>()
    val ingredients = mutableMapOf<String, List<Chemical>>()

    File("input.txt").forEachLine { line ->
        val parts = line.split(" => ")
        val output = parseChemical(parts[1])
        val inputs = parts[0].split(", ").map { parseChemical(it) }
        reactions[output.name] = output
        ingredients[output.name] = inputs
    }

    val oreAvailable = 1000000000000L
    println(maxFuel(reactions, ingredients, oreAvailable))
}

fun calculateOre(chem: String, amount: Long, reactions: Map<String, Chemical>, ingredients: Map<String, List<Chemical>>, surplus: MutableMap<String, Long>): Long {
    if (chem == "ORE") {
        return amount
    }

    if (surplus[chem] ?: 0 >= amount) {
        surplus[chem] = (surplus[chem] ?: 0) - amount
        return 0
    }

    var remainingAmount = amount - (surplus[chem] ?: 0)
    surplus[chem] = 0
    val reaction = reactions[chem]!!
    val times = (remainingAmount + reaction.amount - 1) / reaction.amount
    var ore = 0L

    for (ingredient in ingredients[chem]!!) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    }

    surplus[chem] = (surplus[chem] ?: 0) + times * reaction.amount - remainingAmount
    return ore
}

fun maxFuel(reactions: Map<String, Chemical>, ingredients: Map<String, List<Chemical>>, oreAvailable: Long): Long {
    var low = 0L
    var high = oreAvailable
    while (low < high) {
        val mid = (low + high + 1) / 2
        if (calculateOre("FUEL", mid, reactions, ingredients, mutableMapOf()) > oreAvailable) {
            high = mid - 1
        } else {
            low = mid
        }
    }
    return low
}
