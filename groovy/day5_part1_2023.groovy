def maps = []
def seeds = []
def currentRanges = []

new File("input.txt").eachLine { line ->
    if (line.contains("map:")) {
        if (currentRanges) {
            maps << currentRanges
            currentRanges = []
        }
    } else if (line.startsWith("seeds:")) {
        def seedStrs = line[7..-1].split()
        seeds = seedStrs.collect { it as long } // Use long instead of int
    } else {
        def numbers = line.split()
        if (numbers.size() == 3) {
            def srcStart = numbers[1] as long // Use long instead of int
            def destStart = numbers[0] as long // Use long instead of int
            def length = numbers[2] as long // Use long instead of int
            currentRanges << [srcStart, destStart, length]
        }
    }
}
maps << currentRanges

def convertNumber(number, ranges) {
    for (r in ranges) {
        if (number >= r[0] && number < r[0] + r[2]) {
            return r[1] + (number - r[0])
        }
    }
    number
}

def minLocation = seeds.collect { seed ->
    def location = seed
    for (m in maps) {
        location = convertNumber(location, m)
    }
    location
}.min()

println minLocation