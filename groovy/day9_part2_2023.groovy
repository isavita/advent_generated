
def input = new File("input.txt").text.trim().split("\n")

def parseInput(input) {
    input.collect { line ->
        line.tokenize(" ").collect { it as Integer }
    }
}

def allZeros(nums) {
    nums.every { it == 0 }
}

def calculateExtrapolation(history) {
    def extrapolations = []
    for (int i = 1; i < history.size(); i++) {
        extrapolations.add(history[i] - history[i-1])
    }
    extrapolations
}

def calculateExtrapolations(history) {
    def extrapolationsSeries = [history]

    (1..<history.size()).each { i ->
        def previousExtrapolations = extrapolationsSeries[i-1]
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries
        }

        def extrapolations = calculateExtrapolation(previousExtrapolations)
        extrapolationsSeries.add(extrapolations)
    }

    extrapolationsSeries
}

def solve(input) {
    def histories = parseInput(input)
    def res = 0

    histories.each { history ->
        def extrapolationsSeries = calculateExtrapolations(history)

        def pastPrediction = 0
        for (int i = extrapolationsSeries.size() - 1; i >= 0; i--) {
            pastPrediction = extrapolationsSeries[i][0] - pastPrediction
        }

        res += pastPrediction
    }

    res
}

println solve(input)
