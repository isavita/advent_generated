
def solve() {
    def lines = new File("input.txt").readLines()
    def availablePatterns = lines[0].split(',').collect { it.trim() }
    def designs = lines[2..-1]
    
    def count = designs.count { design -> canMake(design, availablePatterns) }
    println count
}

def canMake(design, patterns) {
    def n = design.size()
    def dp = new boolean[n + 1]
    dp[0] = true
    
    for (int i = 1; i <= n; i++) {
        for (def p : patterns) {
            def lp = p.size()
            if (i >= lp && dp[i - lp] && design[i - lp..<i] == p) {
                dp[i] = true
                break
            }
        }
    }
    return dp[n]
}

solve()
