
class Day12HotSprings {

    static long solve(String springs, List<Integer> groups, boolean unfold = false) {
        if (unfold) {
            springs = (springs + "?") * 4 + springs
            groups = groups * 5
        }
        return countArrangements(springs, groups)
    }

    static long countArrangements(String springs, List<Integer> groups) {
        Map<String, Long> cache = [:]
        return countArrangementsRecursive(springs, groups, 0, 0, 0, cache)
    }

    static long countArrangementsRecursive(String springs, List<Integer> groups, int springIndex, int groupIndex, int currentGroupSize, Map<String, Long> cache) {
        String key = "${springIndex}-${groupIndex}-${currentGroupSize}"
        if (cache.containsKey(key)) {
            return cache[key]
        }

        if (springIndex == springs.length()) {
            if (groupIndex == groups.size() && currentGroupSize == 0) {
                return 1
            } else if (groupIndex == groups.size() - 1 && groups[groupIndex] == currentGroupSize) {
                return 1
            } else {
                return 0
            }
        }

        long arrangements = 0
        char currentChar = springs[springIndex]

        if (currentChar == '.' || currentChar == '?') {
            if (currentGroupSize == 0) {
                arrangements += countArrangementsRecursive(springs, groups, springIndex + 1, groupIndex, 0, cache)
            } else if (groupIndex < groups.size() && groups[groupIndex] == currentGroupSize) {
                arrangements += countArrangementsRecursive(springs, groups, springIndex + 1, groupIndex + 1, 0, cache)
            }
        }

        if (currentChar == '#' || currentChar == '?') {
            if (groupIndex < groups.size() && currentGroupSize < groups[groupIndex]) {
                arrangements += countArrangementsRecursive(springs, groups, springIndex + 1, groupIndex, currentGroupSize + 1, cache)
            }
        }

        cache[key] = arrangements
        return arrangements
    }

    static void main(String[] args) {
        long totalArrangementsPart1 = 0
        long totalArrangementsPart2 = 0

        new File("input.txt").eachLine { line ->
            def parts = line.split(" ")
            def springs = parts[0]
            def groups = parts[1].split(",").collect { it.toInteger() }

            totalArrangementsPart1 += solve(springs, groups)
            totalArrangementsPart2 += solve(springs, groups, true)
        }

        println "Part 1: ${totalArrangementsPart1}" // Output: 7047
        println "Part 2: ${totalArrangementsPart2}" // Output: 17391848518844
    }
}
