
class Day24 {

    static void main(String[] args) {
        def weights = []
        new File("input.txt").eachLine { line ->
            weights << line.toInteger()
        }

        def result = solve(weights)
        println result
    }
    
    static long solve(List<Integer> weights) {
        def targetWeight = weights.sum() / 3
        def minPackages = Integer.MAX_VALUE
        def minQE = Long.MAX_VALUE

        for (int i = 1; i <= weights.size(); i++) {
            def combinations = combinations(weights, i)
            
            for (def combination : combinations) {
                if (combination.sum() == targetWeight) {
                    if (combination.size() < minPackages) {
                        minPackages = combination.size()
                        minQE = product(combination)
                    } else if (combination.size() == minPackages) {
                        def qe = product(combination)
                        minQE = Math.min(minQE, qe)
                    }
                }
            }
             if (minPackages != Integer.MAX_VALUE) {
                  //Optimization: return early once the smallest group size is found.
                  return minQE
             }
        }

       
        return minQE
    }

    static List<List<Integer>> combinations(List<Integer> list, int k) {
        def result = []
        if (k == 0) {
            result << []
            return result
        }
        if (list.isEmpty()) {
            return result
        }

        def first = list[0]
        def rest = list.subList(1, list.size())

        def combsWithoutFirst = combinations(rest, k)
        result.addAll(combsWithoutFirst)

        def combsWithFirst = combinations(rest, k - 1)
        combsWithFirst.each { comb ->
            def newComb = [first] + comb
            result << newComb
        }
        return result
    }
    static long product(List<Integer> list) {
        if (list.isEmpty())
            return 1;
        return list.inject(1L) { acc, val -> acc * val }
    }
}
