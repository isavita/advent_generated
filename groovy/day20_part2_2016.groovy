
import java.nio.file.Files
import java.nio.file.Paths

class FirewallRules {
    static void main(String[] args) {
        def blockedRanges = readBlockedRanges("input.txt")
        def mergedRanges = mergeRanges(blockedRanges)
        
        def lowestAllowedIP = findLowestAllowedIP(mergedRanges)
        println "Lowest allowed IP: ${lowestAllowedIP}"

        def allowedCount = countAllowedIPs(mergedRanges)
        println "Number of allowed IPs: ${allowedCount}"
    }

    static List<List<Long>> readBlockedRanges(String filename) {
        def ranges = []
        Files.lines(Paths.get(filename)).forEach { line ->
            def parts = line.split('-')
            ranges << [parts[0].toLong(), parts[1].toLong()]
        }
        return ranges
    }

    static List<List<Long>> mergeRanges(List<List<Long>> ranges) {
        ranges.sort { it[0] } // Sort by the start of the range
        def merged = []
        
        ranges.each { range ->
            if (merged.isEmpty() || merged.last()[1] < range[0] - 1) {
                merged << range // No overlap, add the range
            } else {
                // Overlap, merge the ranges
                merged.last()[1] = Math.max(merged.last()[1], range[1])
            }
        }
        return merged
    }

    static Long findLowestAllowedIP(List<List<Long>> mergedRanges) {
        long lowestIP = 0
        for (def range : mergedRanges) {
            if (lowestIP < range[0]) {
                return lowestIP // Found the first allowed IP
            }
            lowestIP = Math.max(lowestIP, range[1] + 1) // Move to the next possible IP
        }
        return lowestIP // If all ranges are processed, return the next IP
    }

    static Long countAllowedIPs(List<List<Long>> mergedRanges) {
        long totalIPs = 4294967296 // Total possible IPs (0 to 4294967295)
        long blockedCount = 0
        
        mergedRanges.each { range ->
            blockedCount += (range[1] - range[0] + 1) // Count blocked IPs in the range
        }
        
        return totalIPs - blockedCount // Allowed IPs = Total - Blocked
    }
}
