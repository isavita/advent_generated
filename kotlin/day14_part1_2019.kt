import java.io.File

data class Chemical(val name: String, val amount: Int)

fun parseChemical(s: String): Chemical {
    val parts = s.split(" ")
    val amount = parts[0].toInt()
    return Chemical(parts[1], amount)
}

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    val reactions = mutableMapOf<String, Chemical>()
    val ingredients = mutableMapOf<String, List<Chemical>>()

    for (line in lines) {
        val parts = line.split(" => ")
        val output = parseChemical(parts[1])
        val inputs = parts[0].split(", ").map { parseChemical(it) }
        reactions[output.name] = output
        ingredients[output.name] = inputs
    }

    println(calculateOre("FUEL", 1, reactions, ingredients, mutableMapOf()))
}

fun calculateOre(chem: String, amount: Int, reactions: Map<String, Chemical>, ingredients: Map<String, List<Chemical>>, surplus: MutableMap<String, Int>): Int {
    if (chem == "ORE") {
        return amount
    }

    if (surplus[chem] ?: 0 >= amount) {
        surplus[chem] = surplus[chem]!! - amount
        return 0
    }

    var ore = 0
    var newAmount = amount - (surplus[chem] ?: 0)
    surplus[chem] = 0

    val reaction = reactions[chem]!!
    val times = (newAmount + reaction.amount - 1) / reaction.amount

    for (ingredient in ingredients[chem]!!) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    }

    surplus[chem] = (surplus[chem] ?: 0) + times * reaction.amount - newAmount
    return ore
}