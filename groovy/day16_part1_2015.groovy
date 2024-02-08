
def input = new File("input.txt").readLines()

def mfcsam = [
    children: 3,
    cats: 7,
    samoyeds: 2,
    pomeranians: 3,
    akitas: 0,
    vizslas: 0,
    goldfish: 5,
    trees: 3,
    cars: 2,
    perfumes: 1
]

def sue = 0

input.each { line ->
    def parts = line.split(": ", 2)
    def sueNum = parts[0].split(" ")[1] as int
    def properties = parts[1].split(", ")

    def valid = true
    properties.each { property ->
        def prop = property.split(": ")
        if (mfcsam[prop[0]] != prop[1].toInteger()) {
            valid = false
        }
    }

    if (valid) {
        sue = sueNum
    }
}

println sue
