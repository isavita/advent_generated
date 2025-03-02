
class FractalArt {

    static void main(String[] args) {
        def rules = readRules("input.txt")
        def grid = [
            ".#.".toList(),
            "..#".toList(),
            "###".toList()
        ]

        grid = enhance(grid, rules, 5)
        println "Part 1: ${countPixels(grid)}"

        grid = enhance(grid, rules, 13)
        println "Part 2: ${countPixels(grid)}"
    }

    static List<String> enhance(List<String> grid, Map<String, String> rules, int iterations) {
        def currentGrid = grid
        for (int i = 0; i < iterations; i++) {
            currentGrid = iterate(currentGrid, rules)
        }
        return currentGrid
    }

    static Map<String, String> readRules(String filename) {
        def rules = [:]
        new File(filename).eachLine { line ->
            def parts = line.split(" => ")
            rules[parts[0]] = parts[1]
        }
        return rules
    }

    static List<String> iterate(List<String> grid, Map<String, String> rules) {
        int size = grid.size()
        int subgridSize = size % 2 == 0 ? 2 : 3
        int newSubgridSize = subgridSize + 1
        int numSubgrids = size / subgridSize

        List<String> newGrid = []
        for (int i = 0; i < numSubgrids * newSubgridSize; i++) {
            newGrid.add("".padRight(numSubgrids * newSubgridSize, ' '))
        }

        for (int i = 0; i < numSubgrids; i++) {
            for (int j = 0; j < numSubgrids; j++) {
                List<String> subgrid = []
                for (int x = i * subgridSize; x < (i + 1) * subgridSize; x++) {
                    StringBuilder row = new StringBuilder()
                    for (int y = j * subgridSize; y < (j + 1) * subgridSize; y++) {
                        row.append(grid[x][y])
                    }
                    subgrid.add(row.toString())
                }

                String subgridString = subgrid.join("/")
                String newSubgridString = findRule(subgridString, rules)

                def newSubgridRows = newSubgridString.split("/")

                for (int x = 0; x < newSubgridSize; x++) {
                    for (int y = 0; y < newSubgridSize; y++) {
                        newGrid[i * newSubgridSize + x] = newGrid[i * newSubgridSize + x].substring(0, j * newSubgridSize + y) +
                                newSubgridRows[x][y] +
                                newGrid[i * newSubgridSize + x].substring(j * newSubgridSize + y + 1)
                    }
                }
            }
        }

        return newGrid.collect { it.toList() }
    }

    static String findRule(String subgrid, Map<String, String> rules) {
        if (rules.containsKey(subgrid)) {
            return rules[subgrid]
        }

        // Rotations and flips
        for (int i = 0; i < 4; i++) {
            subgrid = rotate(subgrid)
            if (rules.containsKey(subgrid)) {
                return rules[subgrid]
            }
            String flippedSubgrid = flip(subgrid)
            if (rules.containsKey(flippedSubgrid)) {
                return rules[flippedSubgrid]
            }
        }
        throw new IllegalArgumentException("No rule found for subgrid: $subgrid")
    }

    static String rotate(String pattern) {
        def rows = pattern.split("/")
        int size = rows.size()
        List<String> newRows = []
        for (int i = 0; i < size; i++) {
            newRows.add("".padRight(size, ' '))
        }
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                newRows[j] = newRows[j].substring(0, size - 1 - i) + rows[i][j] + newRows[j].substring(size - i)
            }
        }
        return newRows.join("/")
    }

    static String flip(String pattern) {
        return pattern.split("/").collect { it.reverse() }.join("/")
    }

    static int countPixels(List<String> grid) {
        int count = 0
        grid.each { row ->
            row.each { pixel ->
                if (pixel == '#') {
                    count++
                }
            }
        }
        return count
    }
}
