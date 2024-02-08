
def input = new File("input.txt").text

def removeCanceledCharacters = { str ->
    def cleaned = ""
    def ignoreNext = false
    str.each {
        if (ignoreNext) {
            ignoreNext = false
        } else if (it == "!") {
            ignoreNext = true
        } else {
            cleaned += it
        }
    }
    cleaned
}

def cleanedInput = removeCanceledCharacters(input)

def scoreGroups = { str, depth = 1 ->
    def score = 0
    def totalScore = 0
    def garbageCount = 0
    def inGarbage = false

    str.each {
        if (inGarbage) {
            if (it == ">") {
                inGarbage = false
            } else {
                garbageCount++
            }
        } else {
            if (it == "{") {
                score++
                totalScore += score
            } else if (it == "<") {
                inGarbage = true
            } else if (it == "}") {
                score--
            }
        }
    }

    [totalScore, garbageCount]
}

def (totalScore, garbageCount) = scoreGroups(cleanedInput)

println totalScore
println garbageCount
