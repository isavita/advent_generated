def main():
    name_to_idx = {}
    adj = []
    def idx(name):
        if name not in name_to_idx:
            name_to_idx[name] = len(adj)
            adj.append([])
        return name_to_idx[name]
    with open("input.txt") as f:
        for line in f:
            parts = line.split(":")
            if len(parts) < 2:
                continue
            u = idx(parts[0].strip())
            for t in parts[1].strip().split():
                adj[u].append(idx(t))
    start = idx("you")
    end = idx("out")
    memo = [-1] * len(adj)
    import sys
    sys.setrecursionlimit(10**6)
    def dfs(u):
        if u == end:
            return 1
        if memo[u] != -1:
            return memo[u]
        total = 0
        for v in adj[u]:
            total += dfs(v)
        memo[u] = total
        return total
    print(dfs(start))

if __name__ == "__main__":
    main()
