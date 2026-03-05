
BEGIN {
    while ((getline < "input.txt") > 0) {
        exists[$1] = 1
        if ($1 > max) max = $1
    }
    dp[0] = 1
    for (i = 1; i <= max; i++) {
        if (exists[i]) {
            dp[i] = dp[i-1] + dp[i-2] + dp[i-3]
        }
    }
    printf "%.0f\n", dp[max]
}
