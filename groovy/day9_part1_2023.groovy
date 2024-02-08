
def parseInput(input) {
    def histories = []
    input.each { line ->
        def numbers = line.tokenize(" ").collect { it as Integer }
        histories << numbers
    }
    return histories
}

def allZeros(nums) {
    nums.every { it == 0 }
}

def calculateExtrapolation(history) {
    def extrapolations = []
    (1..history.size() - 1).each { i ->
        extrapolations << history[i] - history[i - 1]
    }
    return extrapolations
}

def calculateExtrapolations(history) {
    def extrapolationsSeries = [history]

    for (i in 1..history.size() - 1) {
        def previousExtrapolations = extrapolationsSeries[i - 1]
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries
        }

        def extrapolations = calculateExtrapolation(previousExtrapolations)
        extrapolationsSeries << extrapolations
    }

    return extrapolationsSeries
}

def solve(input) {
    def histories = parseInput(input)
    def res = 0

    histories.each { history ->
        def extrapolationsSeries = calculateExtrapolations(history)

        def futurePrediction = 0
        for (i in extrapolationsSeries.size() - 1..0) {
            futurePrediction = extrapolationsSeries[i][-1] + futurePrediction
        }

        res += futurePrediction
    }

    return res
}

def input = new File("input.txt").text.readLines()
println solve(input)
