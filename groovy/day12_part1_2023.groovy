def parseInput(input) {
    def rows = []
    input.each { line ->
        def parts = line.split(" ")
        def springs = parts[0]
        def ints = parts[1].split(",").collect { it as int }

        def row = [springs: springs, group: ints]
        rows << row
    }
    rows
}

def countArrangementsRecursive(row, iSprings, iGroup, iContiguousDamaged, cache) {
    if (iSprings == row.springs.size()) {
        if (iGroup == row.group.size() && iContiguousDamaged == 0) {
            return 1
        } else if (iGroup == row.group.size() - 1 && iContiguousDamaged == row.group[iGroup]) {
            return 1
        }
        return 0
    }

    def cacheKey = [iSprings, iGroup, iContiguousDamaged]
    if (cache.containsKey(cacheKey)) {
        return cache[cacheKey]
    }

    def res = 0
    def springChar = row.springs[iSprings]
    if (springChar == '.' || springChar == '?') {
        if (iContiguousDamaged == 0) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache)
        } else if (iContiguousDamaged == row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache)
        }
    }
    if (springChar == '#' || springChar == '?') {
        if (iGroup < row.group.size() && iContiguousDamaged < row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache)
        }
    }

    cache[cacheKey] = res
    res
}

def countArrangements(row) {
    countArrangementsRecursive(row, 0, 0, 0, [:])
}

def unfoldRow(row, unfoldingFactor) {
    def newRow = [springs: row.springs, group: row.group]

    (1..<unfoldingFactor).each { i ->
        newRow.springs += "?" + row.springs
        newRow.group.addAll(row.group)
    }

    newRow
}

def solve(input) {
    def rows = parseInput(input)

    def res = 0
    rows.each { row ->
        res += countArrangements(row)
    }

    res
}

def readFile(fileName) {
    new File(fileName).text.split("\n")
}

def input = readFile("input.txt")
println solve(input)