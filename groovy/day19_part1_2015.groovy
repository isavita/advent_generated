
def input = new File("input.txt").readLines()

def replacements = []
def medicine = ""

input.each { line ->
    if (line.contains("=>")) {
        def parts = line.split(" => ")
        replacements << [parts[0], parts[1]]
    } else if (line != "") {
        medicine = line
    }
}

def distinctMolecules = [:]

replacements.each { replacement ->
    def pattern = replacement[0]
    def target = replacement[1]
    
    def index = 0
    while ((index = medicine.indexOf(pattern, index)) != -1) {
        def newMolecule = medicine[0..index - 1] + target + medicine[index + pattern.length()..-1]
        distinctMolecules[newMolecule] = true
        index++
    }
}

println distinctMolecules.size()
