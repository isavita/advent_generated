
class PlutonianPebbles {

    static List<Long> transformStones(List<Long> stones) {
        List<Long> newStones = []
        stones.each { stone ->
            if (stone == 0L) {
                newStones << 1L
            } else if (String.valueOf(stone).length() % 2 == 0) {
                String stoneStr = String.valueOf(stone)
                int mid = stoneStr.length() / 2
                newStones << Long.parseLong(stoneStr.substring(0, mid))
                newStones << Long.parseLong(stoneStr.substring(mid))
            } else {
                newStones << stone * 2024L
            }
        }
        return newStones
    }

    static void main(String[] args) {
        // Read input from input.txt
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println "Error: input.txt not found."
            return
        }

        List<Long> stones = []
        inputFile.eachLine { line ->
            stones = line.split("\\s+").collect { it.toLong() }
        }
        
        // Blink 25 times
        for (int i = 0; i < 25; i++) {
            stones = transformStones(stones)
        }
        
        //println(stones) // uncomment this line to verify the transformation.
        println stones.size()  // Print the number of stones
    }
}
