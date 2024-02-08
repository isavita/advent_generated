def inputFile = new File("input.txt")
def depths = inputFile.readLines().collect { it.toInteger() }

def count = 0
for (int i = 1; i < depths.size(); i++) {
    if (depths[i] > depths[i-1]) {
        count++
    }
}

println count