
class Main {
    static void main(String[] args) {
        def lines = new File('input.txt').readLines()
        def ranges = []
        def i = 0
        while (i < lines.size() && lines[i].trim()) {
            def (a,b) = lines[i].trim().split('-')
            ranges << [a.toLong(), b.toLong()]
            i++
        }
        if (ranges) {
            ranges.sort { it[0] }
            def merged = []
            ranges.each { r ->
                if (!merged) merged << r
                else if (r[0] > merged[-1][1]) merged << r
                else if (r[1] > merged[-1][1]) merged[-1][1] = r[1]
            }
            ranges = merged
        }
        i++ // skip blank line
        long count = 0
        while (i < lines.size()) {
            def s = lines[i].trim()
            if (s) {
                long id = s.toLong()
                int l = 0, r = ranges.size()
                while (l < r) {
                    int m = (l + r) >>> 1
                    if (id < ranges[m][0]) r = m
                    else if (id > ranges[m][1]) l = m + 1
                    else { count++; break }
                }
            }
            i++
        }
        println "Number of fresh ingredients: $count"
    }
}
