import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val replacements = mutableListOf<String>()
    var molecule = ""

    for (line in lines) {
        if (line.isBlank()) {
            continue
        }
        if (line.contains(" => ")) {
            replacements.add(line)
        } else {
            molecule = line
        }
    }

    val molecules = mutableSetOf<String>()
    for (replacement in replacements) {
        val parts = replacement.split(" => ")
        for (i in 0 until molecule.length) {
            if (molecule.substring(i).startsWith(parts[0])) {
                val newMolecule = molecule.substring(0, i) + parts[1] + molecule.substring(i + parts[0].length)
                molecules.add(newMolecule)
            }
        }
    }

    println(molecules.size)
}