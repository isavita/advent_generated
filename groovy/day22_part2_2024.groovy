
def nextSecret(int s) {
    int mod = 1 << 24
    s ^= (s * 64) & (mod - 1)
    s ^= (s >>> 5) & (mod - 1)
    s ^= (s * 2048) & (mod - 1)
    return s
}

def encodeChange4(int c1, int c2, int c3, int c4) {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 361 + (c4 + 9) * 6859
}

def solve() {
    int numSteps = 2000
    int patternCount = 19 * 19 * 19 * 19

    def initials = new File("input.txt").readLines().collect { it.toInteger() }

    def buyers = initials.collect { initVal ->
        def prices = []
        int s = initVal
        (numSteps + 1).times {
            prices << s % 10
            s = nextSecret(s)
        }
        def changes = (0..<numSteps).collect { prices[it + 1] - prices[it] }
        [prices, changes]
    }

    def globalSum = new int[patternCount]

    buyers.each { buyer ->
        def prices = buyer[0]
        def changes = buyer[1]
        def localPrice = new int[patternCount].with { Arrays.fill(it, -1); it }
        (numSteps - 3).times { i ->
            int c1 = changes[i]
            int c2 = changes[i + 1]
            int c3 = changes[i + 2]
            int c4 = changes[i + 3]

            if (-9 <= c1 && c1 <= 9 && -9 <= c2 && c2 <= 9 && -9 <= c3 && c3 <= 9 && -9 <= c4 && c4 <= 9) {
                int idx = encodeChange4(c1, c2, c3, c4)
                if (localPrice[idx] < 0) {
                    localPrice[idx] = prices[i + 4]
                }
            }
        }
        localPrice.eachWithIndex { p, idx ->
            if (p >= 0) {
                globalSum[idx] += p
            }
        }
    }
    println globalSum.max()
}

solve()
