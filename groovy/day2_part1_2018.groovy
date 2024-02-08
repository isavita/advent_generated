
def file = new File("input.txt")
def twoCount = 0
def threeCount = 0

file.eachLine { line ->
    def counts = countTwosAndThrees(line)
    if (counts[0]) {
        twoCount++
    }
    if (counts[1]) {
        threeCount++
    }
}

def checksum = twoCount * threeCount
println checksum

def countTwosAndThrees(id) {
    def charCount = [:].withDefault{0}
    id.each {
        charCount[it]++
    }

    def hasTwos = false
    def hasThrees = false
    charCount.each { entry ->
        if (entry.value == 2) {
            hasTwos = true
        } else if (entry.value == 3) {
            hasThrees = true
        }
    }
    return [hasTwos, hasThrees]
}
