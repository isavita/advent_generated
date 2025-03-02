
class SleighBalancer {

    static long findOptimalQE(List<Long> weights, int numGroups) {
        long targetWeight = weights.sum() / numGroups
        int minSize = Integer.MAX_VALUE
        long minQE = Long.MAX_VALUE

        // Iterate through all possible combinations for the first group
        for (int i = 1; i <= weights.size(); i++) {
            List<List<Long>> combinations = combinations(weights, i)
            for (List<Long> group1 : combinations) {
                if (group1.sum() == targetWeight) {
                    if (group1.size() < minSize) {
                        minSize = group1.size()
                        minQE = group1.inject(1L) { acc, val -> acc * val }
                    } else if (group1.size() == minSize) {
                        long qe = group1.inject(1L) { acc, val -> acc * val }
                        minQE = Math.min(minQE, qe)
                    }
                    // Optimization: Early exit if a smaller group size is found in a sorted combination.
                    // No longer needed with the refactor and combination optimization.
                }
            }
            // Optimization: since we are incrementing the number of elements.
            // the first group found will always be the minimal.
            if (minSize != Integer.MAX_VALUE)
                return minQE;
        }
        return minQE
    }
    
    // Function to determine if the rest of weights can be distributed.
    static boolean canDistribute(List<Long> remainingWeights, long targetWeight, int remainingGroups) {
        if (remainingGroups == 1) {
            return remainingWeights.sum() == targetWeight
        }
        
        for (int i = 1; i <= remainingWeights.size(); i++) {
             List<List<Long>> combinations = combinations(remainingWeights, i)
             for (List<Long> group : combinations) {
                 if (group.sum() == targetWeight) {
                    List<Long> nextRemaining = new ArrayList<>(remainingWeights)
                    group.each { nextRemaining.remove(it) }  // Efficient removal
                    if (canDistribute(nextRemaining, targetWeight, remainingGroups - 1)) {
                        return true
                    }
                 }
             }
        }
        return false;
    }
    

    static List<List<Long>> combinations(List<Long> elements, int k) {
        List<List<Long>> result = []
        if (k == 0) {
            result.add([])
            return result
        }
        if (elements.isEmpty()) {
            return result
        }

        Long first = elements.first()
        List<Long> rest = elements.subList(1, elements.size())

        // Combinations including the first element
        combinations(rest, k - 1).each { subCombination ->
            List<Long> combination = new ArrayList<>(subCombination)
            combination.add(0, first) // Add at the beginning for consistent order
            result.add(combination)
        }

        // Combinations excluding the first element
        result.addAll(combinations(rest, k))

        return result
    }



    static void main(String[] args) {
        File inputFile = new File("input.txt")
        if (!inputFile.exists()) {
            println("Error: input.txt not found.")
            return
        }

        List<Long> weights = inputFile.readLines().collect { it.toLong() }
        // Sort weights to potentially find smaller combinations earlier. No longer needed because the combinations size is increasing by iteration.
        //weights.sort { a, b -> b <=> a }  //descending.

        // Part 1
        long optimalQEPart1 = findOptimalQE(weights, 3)
        println("Part 1 - Quantum Entanglement: " + optimalQEPart1)

        // Part 2
        long optimalQEPart2 = findOptimalQE(weights, 4)
        println("Part 2 - Quantum Entanglement: " + optimalQEPart2)
    }
}
