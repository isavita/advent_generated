def polymer = new File("input.txt").text.trim()

def reactPolymer(polymer) {
    def result = []

    polymer.each {
        if (result && (result.last().toLowerCase() == it.toLowerCase()) && (result.last() != it)) {
            result.remove(result.size() - 1)
        } else {
            result.add(it)
        }
    }

    return result.size()
}

println reactPolymer(polymer)