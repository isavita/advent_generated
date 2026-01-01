import sys
from collections import defaultdict

def main():
    name_to_idx = {}
    adj = []
    def idx(name):
        if name not in name_to_idx:
            name_to_idx[name] = len(adj)
            adj.append([])
        return name_to_idx[name]

    with open("input.txt") as f:
        for raw in f:
            line = raw.strip()
            if not line:
                continue
            colon = line.find(':')
            if colon == -1:
                continue
            src = line[:colon].strip()
            u = idx(src)
            for tok in line[colon+1:].split():
                v = idx(tok)
                adj[u].append(v)

    svr = idx("svr")
    dac = idx("dac")
    fft = idx("fft")
    out = idx("out")

    sys.setrecursionlimit(10**6)

    def count_paths(s, t):
        memo = [-1] * len(adj)
        def dfs(u):
            if u == t:
                return 1
            if memo[u] != -1:
                return memo[u]
            total = 0
            for v in adj[u]:
                total += dfs(v)
            memo[u] = total
            return total
        return dfs(s)

    s1 = count_paths(svr, dac) * count_paths(dac, fft) * count_paths(fft, out)
    s2 = count_paths(svr, fft) * count_paths(fft, dac) * count_paths(dac, out)

    print(f"Paths (svr->dac->fft->out): {s1}")
    print(f"Paths (svr->fft->dac->out): {s2}")
    print(f"Total paths visiting both: {s1 + s2}")

if __name__ == "__main__":
    main()
