
def solvePart1(input) {
    input.collect { line ->
        def (winning, mine) = line.split(": ")[1].split(" \\| ")
        def winningNumbers = winning.split().collect { it.toInteger() }
        def myNumbers = mine.split().collect { it.toInteger() }
        def matches = myNumbers.count { winningNumbers.contains(it) }
        matches > 0 ? (1 << (matches - 1)) : 0
    }.sum()
}


def solvePart2(input) {
    def cards = input.collect { line ->
        def (winning, mine) = line.split(": ")[1].split(" \\| ")
        def winningNumbers = winning.split().collect { it.toInteger() }
        def myNumbers = mine.split().collect { it.toInteger() }
        [winningNumbers, myNumbers, 1] // Add count of 1 for initial card
    }

    def cardCounts = cards.collect { it[2] }

    for (int i = 0; i < cards.size(); i++) {
        def matches = cards[i][1].count { cards[i][0].contains(it) }
        for (int j = i + 1; j <= i + matches && j < cards.size(); j++) {
            cardCounts[j] += cardCounts[i]
        }
    }
    cardCounts.sum()
}


def input = new File("input.txt").readLines()

println "Part 1: ${solvePart1(input)}"
println "Part 2: ${solvePart2(input)}"

