def inputFile = new File("input.txt")
def lines = inputFile.readLines()

def count = 0

lines.each { line ->
    def sections = line.split(",")
    def range1 = sections[0].tokenize("-").collect { it as int }
    def range2 = sections[1].tokenize("-").collect { it as int }

    if ((range1[0] <= range2[0] && range1[1] >= range2[1]) || (range2[0] <= range1[0] && range2[1] >= range1[1])) {
        count++
    }
}

println count