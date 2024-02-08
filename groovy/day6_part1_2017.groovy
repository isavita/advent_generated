def input = new File("input.txt").text.tokenize().collect { it as int }
def seenConfigurations = [:]
def cycles = 0

while (!seenConfigurations.containsKey(input.toString())) {
    seenConfigurations[input.toString()] = cycles
    def maxIndex = input.indexOf(input.max())
    def blocks = input[maxIndex]
    input[maxIndex] = 0
    
    while (blocks > 0) {
        maxIndex = (maxIndex + 1) % input.size()
        input[maxIndex]++
        blocks--
    }
    
    cycles++
}

println cycles