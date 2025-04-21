
import java.nio.file.Files
import java.nio.file.Paths

class Solution {

    static String trimLeadingZeros(String s) {
        s.replaceFirst(/^0+(?!$)/, '')
    }

    static List<String> splitStone(String s) {
        def mid = s.length() / 2
        def left = trimLeadingZeros(s[0..<mid])
        def right = trimLeadingZeros(s[mid..<s.length()])
        [(left ?: "0"), (right ?: "0")]
    }

    static void main(String[] args) {
        def filePath = Paths.get("input.txt")
        def lines = Files.readAllLines(filePath)

        if (lines.isEmpty()) {
            println 0
            return
        }

        def stones = lines[0].split()

        def stonesMap = new HashMap<String, Long>().with {
            stones.each { put(it, getOrDefault(it, 0L) + 1) }
            it
        }

        def steps = 75
        steps.times {
            def newStonesMap = new HashMap<String, Long>()
            stonesMap.each { stone, count ->
                if (stone == "0") {
                    newStonesMap.compute("1") { k, v -> (v ?: 0L) + count }
                } else if (stone.length() % 2 == 0) {
                    def (left, right) = splitStone(stone)
                    newStonesMap.compute(left) { k, v -> (v ?: 0L) + count }
                    newStonesMap.compute(right) { k, v -> (v ?: 0L) + count }
                } else {
                    def newStone = new BigInteger(stone).multiply(new BigInteger("2024")).toString()
                    newStonesMap.compute(newStone) { k, v -> (v ?: 0L) + count }
                }
            }
            stonesMap = newStonesMap
        }

        def totalStones = stonesMap.values().sum()
        println totalStones
    }
}
