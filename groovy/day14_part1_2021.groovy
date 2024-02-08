
def file = new File("input.txt")
def lines = file.readLines()
def polymer = lines[0]
def rules = [:]

lines[1..-1].each { line ->
    def parts = line.split(" -> ")
    if (parts.size() == 2) {
        rules[parts[0]] = parts[1]
    }
}

(0..9).each {
    polymer = applyInsertion(polymer, rules)
}

def counts = countElements(polymer)
def (min, max) = minMax(counts)

println max - min

def applyInsertion(polymer, rules) {
    def newPolymer = new StringBuilder()
    (0..polymer.size()-2).each { i ->
        newPolymer.append(polymer[i])
        def key = polymer[i..i+1]
        if (rules.containsKey(key)) {
            newPolymer.append(rules[key])
        }
    }
    newPolymer.append(polymer[polymer.size()-1])
    return newPolymer.toString()
}

def countElements(polymer) {
    def counts = [:]
    polymer.each { c ->
        counts[c] = counts.containsKey(c) ? counts[c] + 1 : 1
    }
    return counts
}

def minMax(counts) {
    def min = Integer.MAX_VALUE
    def max = Integer.MIN_VALUE
    counts.each { _, count ->
        min = Math.min(min, count)
        max = Math.max(max, count)
    }
    return [min, max]
}
