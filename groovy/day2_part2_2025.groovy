
import java.math.BigInteger

class Main {
    static void main(String[] args) {
        def data = new File('input.txt').text.replaceAll('[\\r\\n]', '').trim()
        if (!data) return
        def parts = data.split(',')
        def ranges = parts.findAll { it }.collect { p ->
            def b = p.split('-')
            [start: new BigInteger(b[0]), end: new BigInteger(b[1])]
        }
        def ids = new HashSet<String>()
        ranges.each { r ->
            int sLen = r.start.toString().length()
            int eLen = r.end.toString().length()
            for (int totalLen = sLen; totalLen <= eLen; totalLen++) {
                for (int k = 1; k <= totalLen / 2; k++) {
                    if (totalLen % k != 0) continue
                    int reps = totalLen / k
                    def M = BigInteger.ZERO
                    for (int i = 0; i < reps; i++) {
                        M = M.add(BigInteger.TEN.pow(i * k))
                    }
                    def minSeed = BigInteger.TEN.pow(k - 1)
                    def maxSeed = BigInteger.TEN.pow(k).subtract(BigInteger.ONE)
                    def targetMin = r.start.add(M).subtract(BigInteger.ONE).divide(M)
                    def targetMax = r.end.divide(M)
                    def startSeed = targetMin.max(minSeed)
                    def endSeed = targetMax.min(maxSeed)
                    if (startSeed.compareTo(endSeed) <= 0) {
                        def cur = startSeed
                        while (cur.compareTo(endSeed) <= 0) {
                            ids.add(cur.multiply(M).toString())
                            cur = cur.add(BigInteger.ONE)
                        }
                    }
                }
            }
        }
        def sum = ids.collect { new BigInteger(it) }.inject(BigInteger.ZERO) { a, b -> a.add(b) }
        println "Sum of invalid IDs: ${sum}"
    }
}
