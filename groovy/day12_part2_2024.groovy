
def solve() {
    def input = new File("input.txt").text.trim().split('\n')
    def graph = input.collect { it.split('') }
    def H = graph.size()
    def W = graph[0].size()

    def moves = [
        [x: -1, y: 0],
        [x: 0, y: -1],
        [x: 1, y: 0],
        [x: 0, y: 1]
    ]

    def sum = 0

    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++) {
            if (graph[y][x] == '.') continue

            def area = 0
            def target = graph[y][x]
            def visited = [:]
            def side = [:]

            def search
            search = { int cx, int cy, int labelIndex ->
                if (cx < 0 || cx >= W || cy < 0 || cy >= H || graph[cy][cx] != target) {
                    if (labelIndex != -1 && !visited["${cx},${cy}"]) {
                        saveOuter(labelIndex, side, cx, cy)
                    }
                    return
                }

                visited["${cx},${cy}"] = true
                area++
                graph[cy][cx] = '.'

                moves.eachWithIndex { move, index ->
                    search(cx + move.x, cy + move.y, index)
                }
            }

            search(x, y, -1)
            def outer = countOuter(side, W, H)
            sum += area * outer
        }
    }
    println sum
}

def saveOuter(int labelIndex, Map<Integer, Set<String>> side, int x, int y) {
    def key
    if (labelIndex == 1 || labelIndex == 3) {
        key = "${y}:${x}"
    } else {
        key = "${x}:${y}"
    }

    if (!side.containsKey(labelIndex)) {
        side[labelIndex] = new HashSet<>()
    }
    side[labelIndex].add(key)
}

def countOuter(Map<Integer, Set<String>> side, int W, int H) {
    def outer = 0
    side.each { labelIndex, keys ->
        def sortedKeys = keys.sort { a, b ->
            def partsA = a.split(':').collect { it as int }
            def partsB = b.split(':').collect { it as int }
            if (partsA[0] == partsB[0]) {
                return partsA[1] <=> partsB[1]
            }
            return partsA[0] <=> partsB[0]
        }

        def temp = []
        sortedKeys.each { current ->
            def parts = current.split(':').collect { it as int }
            if (!check(temp, parts[0], parts[1], W, H)) {
                outer++
            }
            temp << current
        }
    }
    return outer
}

def check(List<String> ary, int i, int j, int W, int H) {
    def search = [
        "${i}:${j - 1}",
        "${i}:${j + 1}"
    ]
    search.any { s -> ary.contains(s) }
}

solve()
