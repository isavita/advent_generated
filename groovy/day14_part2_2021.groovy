
def solve() {
    def (template, rules) = readInput("input.txt")
    def pairCounts = [:]
    (0..<template.length()-1).each { i ->
        pairCounts[template.substring(i, i+2)] = pairCounts.getOrDefault(template.substring(i, i+2), 0L) + 1
    }

    (0..<40).each { step ->
        def newPairCounts = [:]
        pairCounts.each { pair, count ->
            def insert = rules.get(pair)
            if (insert) {
                newPairCounts[pair[0] + insert] = newPairCounts.getOrDefault(pair[0] + insert, 0L) + count
                newPairCounts[insert + pair[1]] = newPairCounts.getOrDefault(insert + pair[1], 0L) + count
            } else {
                newPairCounts[pair] = newPairCounts.getOrDefault(pair, 0L) + count
            }
        }
        pairCounts = newPairCounts
    }

    def elementCounts = [:]
    pairCounts.each { pair, count ->
        elementCounts[pair[0]] = elementCounts.getOrDefault(pair[0], 0L) + count
    }
    elementCounts[template[-1]]++

    def maxCount = elementCounts.values().max()
    def minCount = elementCounts.values().min()

    println maxCount - minCount
}


def readInput(filename) {
    def template = ""
    def rules = [:]
    new File(filename).eachLine { line ->
        if (template == "") {
            template = line
        } else if (line) {
            def (pair, insert) = line.split(" -> ")
            rules[pair] = insert
        }
    }
    return [template, rules]
}

solve()
