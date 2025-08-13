
#!/usr/bin/awk -f
BEGIN {
    target = 150
    size   = 0

    # Read all container sizes from input.txt
    while ((getline line < "input.txt") > 0) {
        containers[++size] = line + 0
    }
    close("input.txt")

    # Dynamic‑programming subset‑sum count
    dp[0] = 1
    for (i = 1; i <= size; i++) {
        c = containers[i]
        for (t = target; t >= c; t--) {
            dp[t] += dp[t - c]
        }
    }

    # Output the number of combinations that sum to 150
    print dp[target]
}
