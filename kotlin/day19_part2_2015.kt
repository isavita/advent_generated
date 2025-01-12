
import java.io.File
import kotlin.random.Random
import kotlin.system.measureTimeMillis

fun main() {
    val input = File("input.txt").readText().trim()
    println(solve(input))
}

fun parseInput(input: String): Pair<Map<String, List<String>>, List<String>> {
    val blocks = input.split("\n\n")
    val startingMaterial = splitMolecules(blocks[1])

    val graph = mutableMapOf<String, MutableList<String>>()
    blocks[0].split("\n").forEach { line ->
        val parts = line.split(" => ")
        graph.computeIfAbsent(parts[0]) { mutableListOf() }.add(parts[1])
    }

    return graph to startingMaterial
}

fun splitMolecules(input: String): List<String> {
    val molecules = mutableListOf<String>()
    var currentMolecule = ""
    for (char in input) {
        if (char.isUpperCase()) {
            if (currentMolecule.isNotEmpty()) {
                molecules.add(currentMolecule)
            }
            currentMolecule = char.toString()
        } else {
            currentMolecule += char
        }
    }
    if (currentMolecule.isNotEmpty()) {
        molecules.add(currentMolecule)
    }
    return molecules
}

fun solve(input: String): Int {
    val (reverseGraph, startingMols) = parseInput(input)

    val productToReactant = mutableMapOf<String, String>()
    for ((react, products) in reverseGraph) {
        for (p in products) {
            if (productToReactant.containsKey(p)) {
                throw IllegalStateException("Duplicate product found: $p")
            }
            productToReactant[p] = react
        }
    }

    val allProducts = productToReactant.keys.toMutableList()
    val start = startingMols.joinToString("")
    var mol = start
    var steps = 0

    while (mol != "e") {
        var changeMade = false
        for (prod in allProducts) {
            val count = mol.split(prod).size - 1
            if (count <= 0) continue
            changeMade = true
            steps += count
            mol = mol.replace(prod, productToReactant[prod]!!)
            break
        }

        if (!changeMade) {
            allProducts.shuffle()
            mol = start
            steps = 0
        }
    }
    return steps
}
