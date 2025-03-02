
class CrabCups {

    static String solvePart1(String input, int moves) {
        List<Integer> cups = input.collect { it.toInteger() }
        int currentCupIndex = 0

        for (int move = 0; move < moves; move++) {
            int currentCup = cups[currentCupIndex]

            // Pick up three cups
            List<Integer> pickedUp = []
            for (int i = 0; i < 3; i++) {
                pickedUp << cups.remove((currentCupIndex + 1) % cups.size())
            }

            // Find destination
            int destinationCup = currentCup - 1
            while (destinationCup < 1 || pickedUp.contains(destinationCup)) {
                if (destinationCup < 1) {
                    destinationCup = cups.max()
                } else {
                    destinationCup--
                }
            }

            // Find destination index
            int destinationIndex = cups.indexOf(destinationCup)

            // Place picked up cups
            cups.addAll(destinationIndex + 1, pickedUp)

            // Update current cup index.  Adjust for removals and insertions.
            currentCupIndex = (cups.indexOf(currentCup) + 1) % cups.size()

        }

        // Find cup 1 and rotate the list
        int oneIndex = cups.indexOf(1)
        List<Integer> rotatedCups = cups.subList(oneIndex + 1, cups.size()) + cups.subList(0, oneIndex)

        return rotatedCups.join('')
    }
    static long solvePart2(String input, int numCups, int moves) {
        List<Integer> initialCups = input.collect { it.toInteger() }
        int maxInitialCup = initialCups.max()

        // Use a linked list (simulated with an array) for efficient insertion/removal
        int[] nextCup = new int[numCups + 1]

        // Initialize linked list with initial cups
        for (int i = 0; i < initialCups.size() - 1; i++) {
            nextCup[initialCups[i]] = initialCups[i + 1]
        }

        // Complete the linked list up to numCups
        if (initialCups.size() < numCups) {
          nextCup[initialCups.last()] = maxInitialCup + 1;
          for (int i = maxInitialCup + 1; i < numCups; i++) {
                nextCup[i] = i + 1
          }
          nextCup[numCups] = initialCups[0]
        } else {
          nextCup[initialCups.last()] = initialCups[0];
        }


        int currentCup = initialCups[0]

        for (int move = 0; move < moves; move++) {
            // Pick up three cups
            int cup1 = nextCup[currentCup]
            int cup2 = nextCup[cup1]
            int cup3 = nextCup[cup2]
            List<Integer> pickedUp = [cup1, cup2, cup3]

            // Update linked list to remove picked up cups
            nextCup[currentCup] = nextCup[cup3]

            // Find destination
            int destinationCup = currentCup - 1
            while (destinationCup < 1 || pickedUp.contains(destinationCup)) {
                if (destinationCup < 1) {
                    destinationCup = numCups
                } else {
                    destinationCup--
                }
            }

            // Insert picked up cups after destination
            int temp = nextCup[destinationCup]
            nextCup[destinationCup] = cup1
            nextCup[cup3] = temp

            // Move to next current cup
            currentCup = nextCup[currentCup]
        }

        // Find cups after 1
        long cupAfter1 = nextCup[1]
        long cupAfter2 = nextCup[(int)cupAfter1]

        return cupAfter1 * cupAfter2
    }

    static void main(String[] args) {
        def inputFile = new File("input.txt")

        if (!inputFile.exists()) {
            println "Error: input.txt not found"
            return
        }

        String input = inputFile.text.trim()

        println solvePart1(input, 100)
        println solvePart2(input, 1000000, 10000000)
    }
}
