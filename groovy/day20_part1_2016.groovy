import java.util.*

class IpRange {
    long start
    long end
}

List<IpRange> readIPRanges(String filename) {
    File file = new File(filename)
    List<IpRange> ranges = []
    file.eachLine { line ->
        def parts = line.split('-')
        def start = parts[0].toLong()
        def end = parts[1].toLong()
        ranges << new IpRange(start: start, end: end)
    }
    ranges.sort { it.start }
    ranges
}

long findUnblockedIP(List<IpRange> ranges) {
    long currentIP = 0
    ranges.each { range ->
        if (range.start > currentIP) {
            return currentIP
        }
        if (range.end >= currentIP) {
            currentIP = range.end + 1
        }
    }
    currentIP
}

List<IpRange> ipRanges = readIPRanges("input.txt")
long unblockedIP = findUnblockedIP(ipRanges)
println unblockedIP