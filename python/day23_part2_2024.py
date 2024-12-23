
def solve():
    graph = {}
    nodes = set()
    with open("input.txt", "r") as f:
        for line in f:
            a, b = line.strip().split("-")
            graph.setdefault(a, set()).add(b)
            graph.setdefault(b, set()).add(a)
            nodes.add(a)
            nodes.add(b)
    
    best_clique = []
    
    def bron_kerbosch(r, p, x):
        nonlocal best_clique
        if not p and not x:
            if len(r) > len(best_clique):
                best_clique = r[:]
            return
        
        for v in list(p):
            neighbors = graph.get(v, set())
            bron_kerbosch(
                r + [v],
                list(set(p) & neighbors),
                list(set(x) & neighbors)
            )
            p.remove(v)
            x.append(v)
    
    bron_kerbosch([], list(nodes), [])
    best_clique.sort()
    print(",".join(best_clique))

solve()
