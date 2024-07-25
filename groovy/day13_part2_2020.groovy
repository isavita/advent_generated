
import java.nio.file.Files
import java.nio.file.Paths

def readInput(filePath) {
    def lines = Files.readAllLines(Paths.get(filePath))
    return [lines[0].toInteger(), lines[1].split(',').collect { it == 'x' ? null : it.toInteger() }]
}

def findEarliestTimestamp(busIds) {
    long timestamp = 0
    long step = 1

    busIds.eachWithIndex { busId, index ->
        if (busId != null) {
            while ((timestamp + index) % busId != 0) {
                timestamp += step
            }
            step *= busId
        }
    }

    return timestamp
}

def main() {
    def (earliestDeparture, busIds) = readInput('input.txt')
    def earliestTimestamp = findEarliestTimestamp(busIds)
    println "Earliest timestamp: $earliestTimestamp"
}

main()
