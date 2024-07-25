
import java.nio.file.Files
import java.nio.file.Paths

def readInput(filePath) {
    def lines = Files.readAllLines(Paths.get(filePath))
    def replacements = []
    def targetMolecule = ""
    
    lines.each { line ->
        if (line.trim()) {
            if (line.contains("=>")) {
                def parts = line.split("=>").collect { it.trim() }
                replacements << [from: parts[0], to: parts[1]]
            } else {
                targetMolecule = line.trim()
            }
        }
    }
    return [replacements, targetMolecule]
}

def generateDistinctMolecules(replacements, molecule) {
    def distinctMolecules = new HashSet<String>()
    
    replacements.each { replacement ->
        def from = replacement.from
        def to = replacement.to
        
        // Find all occurrences of 'from' in the molecule
        int index = molecule.indexOf(from)
        while (index != -1) {
            // Create a new molecule by replacing 'from' with 'to'
            def newMolecule = molecule.substring(0, index) + to + molecule.substring(index + from.length())
            distinctMolecules.add(newMolecule)
            index = molecule.indexOf(from, index + 1)
        }
    }
    
    return distinctMolecules.size()
}

def fewestStepsToCreateMolecule(replacements, targetMolecule) {
    def reverseMap = [:]
    replacements.each { rep ->
        reverseMap[rep.to] = (reverseMap[rep.to] ?: []) + rep.from
    }
    
    def steps = 0
    def currentMolecule = targetMolecule
    
    // We will keep replacing until we reach 'e'
    while (currentMolecule != 'e') {
        def found = false
        reverseMap.each { to, fromList ->
            fromList.each { from ->
                if (currentMolecule.contains(to)) {
                    currentMolecule = currentMolecule.replaceFirst(to, from)
                    found = true
                    steps++
                }
            }
        }
        if (!found) {
            throw new IllegalStateException("Cannot reduce molecule to 'e'")
        }
    }
    
    return steps
}

def main() {
    def (replacements, targetMolecule) = readInput("input.txt")
    
    // Part 1: Generate distinct molecules
    def distinctCount = generateDistinctMolecules(replacements, targetMolecule)
    println "Distinct molecules generated: $distinctCount"
    
    // Part 2: Fewest steps to create the target molecule from 'e'
    def steps = fewestStepsToCreateMolecule(replacements, targetMolecule)
    println "Fewest steps to create the medicine molecule: $steps"
}

main()
