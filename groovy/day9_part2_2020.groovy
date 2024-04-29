def input = new File('input.txt').text.split('\n')*.toLong()

def findInvalidNumber(def input, preambleSize) {
    def numbers = input[0..preambleSize-1]
    for (int i = preambleSize; i < input.size(); i++) {
        def isValid = false
        for (int j = i - preambleSize; j < i; j++) {
            for (int k = j + 1; k < i; k++) {
                if (input[i] == input[j] + input[k]) {
                    isValid = true
                    break
                }
            }
            if (isValid) break
        }
        if (!isValid) return input[i]
    }
    return null
}

def findContiguousSet(def input, sum) {
    for (int i = 0; i < input.size(); i++) {
        long total = 0
        for (int j = i; j < input.size(); j++) {
            total += input[j]
            if (total == sum) {
                def set = input[i..j]
                return [set.min(), set.max()]
            }
            if (total > sum) break
        }
    }
    return null
}

def part1 = findInvalidNumber(input, 25)
println "Part 1: $part1"

def part2 = findContiguousSet(input, part1)
println "Part 2: ${part2[0] + part2[1]}"