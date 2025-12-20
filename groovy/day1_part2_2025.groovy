import java.nio.file.*
import java.util.stream.*

class Main {
    static void main(String[] args) {
        int currentPos = 50
        int totalZeroHits = 0
        final int dialSize = 100

        Files.lines(Paths.get("input.txt")).forEach { line ->
            line = line.trim()
            if (line.isEmpty()) return

            char direction = line.charAt(0)
            int amount = line.substring(1).toInteger()

            if (direction == 'R') {
                totalZeroHits += (currentPos + amount) / dialSize
                currentPos = (currentPos + amount) % dialSize
            } else if (direction == 'L') {
                totalZeroHits += Math.floorDiv(currentPos - 1, dialSize) - Math.floorDiv(currentPos - amount - 1, dialSize)
                currentPos = (currentPos - amount) % dialSize
                if (currentPos < 0) currentPos += dialSize
            }
        }

        println "The password is: $totalZeroHits"
    }
}