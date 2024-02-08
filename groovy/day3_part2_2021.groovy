
def values = new File("input.txt").readLines()

def filterValues(values, criteria) {
    for (int i = 0; i < values[0].size(); i++) {
        int zeros = 0, ones = 0
        values.each { val ->
            if (val.charAt(i) == '0') {
                zeros++
            } else {
                ones++
            }
        }
        def keep = criteria(zeros, ones)
        values = filterByBit(values, i, keep)
        if (values.size() == 1) {
            break
        }
    }
    return values[0]
}

def filterByBit(values, bitIndex, keep) {
    def filtered = []
    values.each { val ->
        if (val.charAt(bitIndex) == keep) {
            filtered.add(val)
        }
    }
    return filtered
}

def oxygenGeneratorRating = filterValues(values) { zeros, ones ->
    if (zeros > ones) {
        return '0'
    } else {
        return '1'
    }
}
def oxygenGeneratorRatingInt = Long.parseLong(oxygenGeneratorRating, 2)

def co2ScrubberRating = filterValues(values) { zeros, ones ->
    if (zeros <= ones) {
        return '0'
    } else {
        return '1'
    }
}
def co2ScrubberRatingInt = Long.parseLong(co2ScrubberRating, 2)

println oxygenGeneratorRatingInt * co2ScrubberRatingInt
