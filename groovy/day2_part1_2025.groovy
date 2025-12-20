
import java.math.BigInteger

class Main {
    static void main(String[] args) {
        def data = new File('input.txt').text.replaceAll(/[\r\n]/, '').trim()
        def ranges = data.split(/,/).collect { part ->
            def (s, e) = part.split(/-/)
            [new BigInteger(s), new BigInteger(e)]
        }

        def ids = [] as Set

        (1..10).each { k ->
            def mul = BigInteger.TEN.pow(k).add(BigInteger.ONE)
            def minSeed = BigInteger.TEN.pow(k - 1)
            def maxSeed = BigInteger.TEN.pow(k).subtract(BigInteger.ONE)

            ranges.each { start, end ->
                def sMin = start.add(mul).subtract(BigInteger.ONE).divide(mul)
                def sMax = end.divide(mul)

                def from = sMin.max(minSeed)
                def to   = sMax.min(maxSeed)

                if (from <= to) {
                    def cur = from
                    while (cur <= to) {
                        ids << cur.multiply(mul)
                        cur = cur.add(BigInteger.ONE)
                    }
                }
            }
        }

        def sum = ids.inject(BigInteger.ZERO) { acc, v -> acc.add(v) }
        println sum
    }
}
