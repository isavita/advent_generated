
def can_make(design, patterns):
    n = len(design)
    dp = [False] * (n + 1)
    dp[0] = True
    for i in range(1, n + 1):
        for p in patterns:
            lp = len(p)
            if i >= lp and dp[i - lp] and design[i - lp:i] == p:
                dp[i] = True
                break
    return dp[n]

with open("input.txt", "r") as f:
    available_patterns = f.readline().strip().split(",")
    available_patterns = [p.strip() for p in available_patterns]
    f.readline()
    count = 0
    for design in f:
        design = design.strip()
        if can_make(design, available_patterns):
            count += 1
    print(count)
