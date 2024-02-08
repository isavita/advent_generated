
def input = new File("input.txt").text.split("-").collect { it.toInteger() }

def countValidPasswords = { range, strictDouble ->
    def isValid = { num ->
        def strNum = num.toString()
        def hasAdjacent = false
        def hasStrictAdjacent = false
        def neverDecrease = true

        for (int i = 0; i < strNum.size() - 1; i++) {
            if (strNum[i] == strNum[i + 1]) {
                if (strictDouble) {
                    if (i == 0 || strNum[i - 1] != strNum[i]) {
                        if (i == strNum.size() - 2 || strNum[i + 1] != strNum[i + 2]) {
                            hasStrictAdjacent = true
                        }
                    }
                } else {
                    hasAdjacent = true
                }
            }
            if (strNum[i] > strNum[i + 1]) {
                neverDecrease = false
                break
            }
        }

        return (hasAdjacent || hasStrictAdjacent) && neverDecrease
    }

    def count = 0
    (range[0]..range[1]).each {
        if (isValid(it)) {
            count++
        }
    }

    return count
}

def part1 = countValidPasswords(input, false)
def part2 = countValidPasswords(input, true)

println part1
println part2
