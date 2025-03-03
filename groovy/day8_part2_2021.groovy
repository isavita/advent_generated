
def jumbledSevenSegment(String inputData) {
    def parsedInput = inputData.readLines().collect { line ->
        line.split("\\s+\\|\\s+|\\s+").collect { it.sort() }
    }

    def ans = 0
    parsedInput.each { set ->
        def workingSet = set[0..9]
        def indexToCharacters = [""] * 10

        indexToCharacters[1] = workingSet.find { it.length() == 2 }
        indexToCharacters[4] = workingSet.find { it.length() == 4 }
        indexToCharacters[7] = workingSet.find { it.length() == 3 }
        indexToCharacters[8] = workingSet.find { it.length() == 7 }

        workingSet = workingSet.findAll { it != indexToCharacters[1] && it != indexToCharacters[4] && it != indexToCharacters[7] && it != indexToCharacters[8] }

        def zeroThreeOrNine = workingSet.findAll { it.contains(indexToCharacters[1].toList()) }
        indexToCharacters[3] = zeroThreeOrNine.find { it.length() == 5 }
        zeroThreeOrNine.remove(indexToCharacters[3])

        indexToCharacters[9] = zeroThreeOrNine.find { it.contains(indexToCharacters[4].toList()) }
        zeroThreeOrNine.remove(indexToCharacters[9])

        indexToCharacters[0] = zeroThreeOrNine[0]

        workingSet = workingSet.findAll { it != indexToCharacters[0] && it != indexToCharacters[3] && it != indexToCharacters[9] }

        indexToCharacters[6] = workingSet.find { it.length() == 6 }
        workingSet.remove(indexToCharacters[6])

        indexToCharacters[5] = workingSet.find { indexToCharacters[9].contains(it.toList()) }
        workingSet.remove(indexToCharacters[5])

        indexToCharacters[2] = workingSet[0]

        def num = 0
        set[10..13].each { out ->
            num *= 10
            num += indexToCharacters.indexOf(out)
        }
        ans += num
    }

    return ans
}

String.metaClass.contains = { List chars ->
    delegate.toList().containsAll(chars)
}

String.metaClass.sort = { ->
    delegate.toList().sort().join()
}

def main() {
    def inputText = new File("input.txt").text.trim()
    def answer = jumbledSevenSegment(inputText)
    println answer
}

main()
