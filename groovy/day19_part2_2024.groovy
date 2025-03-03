
def countWays(String design, List<String> patterns) {
    int n = design.length()
    def dp = new long[n + 1]
    dp[0] = 1
    for (int i = 1; i <= n; i++) {
        for (String p : patterns) {
            int lp = p.length()
            if (i >= lp && design.substring(i - lp, i) == p) {
                dp[i] += dp[i - lp]
            }
        }
    }
    dp[n]
}

static void main(String[] args) {
    def lines = new File("input.txt").readLines()
    def availablePatterns = lines[0].split(',').collect { it.trim() }
    long totalWays = 0
    for (int i = 2; i < lines.size(); i++) {
        totalWays += countWays(lines[i].trim(), availablePatterns)
    }
    println(totalWays)
}
