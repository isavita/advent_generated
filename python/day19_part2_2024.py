
def solve():
    with open("input.txt", "r") as f:
        available_patterns = f.readline().strip().split(",")
        available_patterns = [p.strip() for p in available_patterns]
        f.readline()
        total_ways = 0
        for design in f:
            design = design.strip()
            total_ways += count_ways(design, available_patterns)
        print(total_ways)

def count_ways(design, patterns):
    n = len(design)
    dp = [0] * (n + 1)
    dp[0] = 1
    for i in range(1, n + 1):
        for p in patterns:
            lp = len(p)
            if i >= lp and design[i - lp:i] == p:
                dp[i] += dp[i - lp]
    return dp[n]

solve()
