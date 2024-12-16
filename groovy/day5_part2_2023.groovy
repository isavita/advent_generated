
class RangeMap {
    long srcStart, destStart, length
}

long reverseConvertNumber(long number, List<RangeMap> ranges) {
    for (int i = ranges.size() - 1; i >= 0; i--) {
        RangeMap r = ranges[i]
        if (number >= r.destStart && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart)
        }
    }
    return number
}

boolean isInSeedRanges(long number, List<List<Long>> ranges) {
    for (List<Long> r : ranges) {
        if (number >= r[0] && number < r[0] + r[1]) {
            return true
        }
    }
    return false
}

def solve() {
    def file = new File("input.txt")
    def lines = file.readLines()

    List<List<Long>> seedRanges = []
    List<RangeMap> currentRanges = []
    List<List<RangeMap>> maps = []

    lines.each { String line ->
        if (line.contains("map:")) {
            if (!currentRanges.isEmpty()) {
                maps.add(currentRanges)
                currentRanges = []
            }
        } else if (line.startsWith("seeds:")) {
            def seedStrs = line.substring(7).split(" ")
            for (int i = 0; i < seedStrs.size(); i += 2) {
                seedRanges.add([seedStrs[i].toLong(), seedStrs[i + 1].toLong()])
            }
        } else {
            def numbers = line.split(" ").findAll { it }
            if (numbers.size() == 3) {
                currentRanges.add(new RangeMap(srcStart: numbers[1].toLong(), destStart: numbers[0].toLong(), length: numbers[2].toLong()))
            }
        }
    }
    if (!currentRanges.isEmpty()) {
        maps.add(currentRanges)
    }

    long location = 0
    while (true) {
        long seed = location
        for (int i = maps.size() - 1; i >= 0; i--) {
            seed = reverseConvertNumber(seed, maps[i])
        }

        if (isInSeedRanges(seed, seedRanges)) {
            println location
            break
        }
        location++
    }
}

solve()
