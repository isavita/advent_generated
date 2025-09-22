class Main {
    static Map buildMap(String regex) {
        def dm = [:]
        def stack = []
        def cp = [x: 0, y: 0]
        for (int i = 0; i < regex.length(); i++) {
            char c = regex.charAt(i)
            if (c == '(') {
                stack << [x: cp['x'], y: cp['y']]
            } else if (c == '|') {
                cp = stack[-1]
            } else if (c == ')') {
                cp = stack[-1]
                stack.removeLast()
            } else {
                def np = move(cp, c)
                if (!dm.containsKey(cp['x'])) dm[cp['x']] = [:]
                if (!dm[cp['x']].containsKey(cp['y'])) dm[cp['x']][cp['y']] = new HashSet<String>()
                dm[cp['x']][cp['y']] << "${np['x']},${np['y']}"
                cp = np
            }
        }
        return dm
    }

    static def move(def p, char dir) {
        switch (dir) {
            case 'N':
                return [x: p['x'], y: p['y'] - 1]
            case 'S':
                return [x: p['x'], y: p['y'] + 1]
            case 'E':
                return [x: p['x'] + 1, y: p['y']]
            case 'W':
                return [x: p['x'] - 1, y: p['y']]
        }
        return p
    }

    static int findFurthestRoom(def dm) {
        def visited = [:]
        def queue = new ArrayDeque<Map>()
        queue.add([x: 0, y: 0])
        int maxDoors = 0

        while (!queue.isEmpty()) {
            def p = queue.removeFirst()
            int px = p['x']
            int py = p['y']
            if (dm.containsKey(px) && dm[px].containsKey(py)) {
                def neighbors = dm[px][py]
                for (String s : neighbors) {
                    def parts = s.split(',')
                    int nx = parts[0] as int
                    int ny = parts[1] as int
                    if (!visited.containsKey(nx)) visited[nx] = [:]
                    if (!visited[nx].containsKey(ny)) {
                        int distFromCurrent = (visited.containsKey(px) && visited[px].containsKey(py)) ? visited[px][py] : 0
                        int nd = distFromCurrent + 1
                        visited[nx][ny] = nd
                        if (nd > maxDoors) maxDoors = nd
                        queue.add([x: nx, y: ny])
                    }
                }
            }
        }
        return maxDoors
    }

    static void main(String[] args) {
        def file = new File("input.txt")
        if (!file.exists()) return
        String input = file.text
        input = input.trim()
        if (input.length() >= 2) {
            String regex = input.substring(1, input.length() - 1)
            def dm = buildMap(regex)
            int ans = findFurthestRoom(dm)
            println(ans)
        } else {
            println(0)
        }
    }
}
Main.main(null)