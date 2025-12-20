import java.nio.file.*

def lines = Files.readAllLines(Paths.get('input.txt'))
def ranges = []
for (line in lines) {
    def s = line.trim()
    if (!s) continue
    def parts = s.split('-')
    if (parts.length != 2) continue
    long a = parts[0] as long
    long b = parts[1] as long
    if (a > b) { def t = a; a = b; b = t }
    ranges << [a, b]
}
if (ranges.isEmpty()) {
    println 'Total fresh IDs: 0'
    return
}
ranges.sort { r1, r2 -> r1[0] == r2[0] ? r1[1] <=> r2[1] : r1[0] <=> r2[0] }

def total = 0L
def curMin = ranges[0][0]
def curMax = ranges[0][1]

ranges[1..-1].each { r ->
    if (r[0] <= curMax) {
        if (r[1] > curMax) curMax = r[1]
    } else {
        total += curMax - curMin + 1
        curMin = r[0]
        curMax = r[1]
    }
}
total += curMax - curMin + 1
println "Total fresh IDs: $total"