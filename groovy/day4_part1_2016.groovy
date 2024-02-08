def input = new File("input.txt").text

def isValidRoom(String room) {
    def parts = room.tokenize("-")
    def name = parts[0..-2].join()
    def sectorIdChecksum = parts[-1].tokenize("[")
    def sectorId = sectorIdChecksum[0] as int
    def checksum = sectorIdChecksum[1][0..-2]

    def letterCount = [:].withDefault{ 0 }
    name.each {
        if (it != '-') {
            letterCount[it]++
        }
    }

    def sortedLetters = letterCount.collect { key, value -> [key, value] }.sort { a, b ->
        a[1] == b[1] ? a[0] <=> b[0] : b[1] <=> a[1]
    }.collect { it[0] }[0..4].join()

    return sortedLetters == checksum
}

def sumOfSectorIds = input.readLines().findAll { isValidRoom(it) }.collect {
    it.tokenize("-")[-1].tokenize("[")[0] as int
}.sum()

println sumOfSectorIds