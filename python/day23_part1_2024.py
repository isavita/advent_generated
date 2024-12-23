
def solve():
    graph = {}
    with open("input.txt", "r") as f:
        for line in f:
            a, b = line.strip().split("-")
            graph.setdefault(a, set()).add(b)
            graph.setdefault(b, set()).add(a)

    computers = list(graph.keys())
    count = 0
    seen = set()

    for i in range(len(computers)):
        for j in range(i + 1, len(computers)):
            for k in range(j + 1, len(computers)):
                c1, c2, c3 = computers[i], computers[j], computers[k]
                if c2 in graph[c1] and c3 in graph[c2] and c3 in graph[c1]:
                    if c1.startswith("t") or c2.startswith("t") or c3.startswith("t"):
                        triplet = tuple(sorted((c1, c2, c3)))
                        if triplet not in seen:
                            seen.add(triplet)
                            count += 1
    print(count)

solve()
