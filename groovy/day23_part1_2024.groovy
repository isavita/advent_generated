
def graph = [:]
new File("input.txt").eachLine { line ->
    def computers = line.split("-")
    if (computers.size() == 2) {
        def (from, to) = computers
        graph.computeIfAbsent(from, {[:]})[to] = true
        graph.computeIfAbsent(to, {[:]})[from] = true
    }
}

def triplets = []
def seen = [:]
def computers = graph.keySet() as List

for (i in 0..<computers.size()) {
    for (j in (i + 1)..<computers.size()) {
        for (k in (j + 1)..<computers.size()) {
            def c1 = computers[i]
            def c2 = computers[j]
            def c3 = computers[k]
            if (graph[c1][c2] && graph[c2][c3] && graph[c1][c3]) {
                if (c1.startsWith("t") || c2.startsWith("t") || c3.startsWith("t")) {
                    def triplet = [c1, c2, c3]
                    triplet.sort()
                    def key = triplet.join(",")
                    if (!seen[key]) {
                        triplets << triplet
                        seen[key] = true
                    }
                }
            }
        }
    }
}

println triplets.size()
