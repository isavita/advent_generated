
def solvePart1() {
    def sum = 0
    new File('input.txt').eachLine { line ->
        def digits = line.findAll { it in '0'..'9' }
        if (digits) {
            sum += (digits.first().toInteger() * 10) + digits.last().toInteger()
        }
    }
    println sum
}

def solvePart2() {
    def sum = 0
    def digitMap = [
        'one': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5,
        'six': 6, 'seven': 7, 'eight': 8, 'nine': 9
    ]
    new File('input.txt').eachLine { line ->
        def firstDigit = findFirstDigit(line, digitMap)
        def lastDigit = findLastDigit(line, digitMap)
        sum += (firstDigit * 10) + lastDigit
    }
    println sum
}

def findFirstDigit(String line, Map<String, Integer> digitMap) {
    def firstIndex = line.size()
    def firstDigit = -1
    
    for (def entry : digitMap) {
        def index = line.indexOf(entry.key)
        if (index != -1 && index < firstIndex) {
            firstIndex = index
            firstDigit = entry.value
        }
    }
    
    for (int i = 0; i < line.size(); i++) {
        if (line[i] in '0'..'9') {
            if (i < firstIndex) {
                firstDigit = line[i].toInteger()
            }
            break
        }
    }
    
    return firstDigit
}

def findLastDigit(String line, Map<String, Integer> digitMap) {
    def lastIndex = -1
    def lastDigit = -1
    
    for (def entry : digitMap) {
        def index = line.lastIndexOf(entry.key)
        if (index != -1 && index > lastIndex) {
            lastIndex = index
            lastDigit = entry.value
        }
    }
    
    for (int i = line.size() - 1; i >= 0; i--) {
        if (line[i] in '0'..'9') {
            if (i > lastIndex) {
                lastDigit = line[i].toInteger()
            }
            break
        }
    }
    
    return lastDigit
}

solvePart1()
solvePart2()
