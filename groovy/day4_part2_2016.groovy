file = new File("input.txt")
file.eachLine { line ->
    if (isRealRoom(line)) {
        decryptedName = decryptName(line)
        if (decryptedName.contains("northpole object")) {
            println getSectorID(line)
            return
        }
    }
}

def isRealRoom(room) {
    parts = room.split(/\[/)
    checksum = parts[1].trim().replaceAll(/\]/, '')
    encryptedName = parts[0].split('-') as List
    encryptedName = encryptedName[0..-2]

    letterCounts = [:]
    encryptedName.each { part ->
        part.each { letter ->
            letterCounts[letter] = (letterCounts[letter] ?: 0) + 1
        }
    }

    counts = letterCounts.sort { a, b -> b.value <=> a.value ?: a.key <=> b.key }.collect { k, v -> [letter: k, count: v] }

    (0..<checksum.size()).every { i ->
        checksum[i] == counts[i].letter
    }
}

def getSectorID(room) {
    parts = room.split('-')
    sectorIDPart = parts[-1]
    sectorID = sectorIDPart.split('\\[')[0] as int
    sectorID
}

def decryptName(room) {
    parts = room.split('-')
    sectorIDPart = parts[-1]
    sectorID = sectorIDPart.split('\\[')[0] as int
    decryptedName = new StringBuilder()

    parts[0..-2].each { part ->
        part.each { letter ->
            if (letter == '-') {
                decryptedName << ' '
            } else {
                shiftedLetter = ((int)letter - (int)'a' + sectorID) % 26 + (int)'a'
                decryptedName << (char)shiftedLetter
            }
        }
        decryptedName << ' '
    }

    decryptedName.toString().trim()
}