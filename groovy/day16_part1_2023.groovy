
def solve() {
    def grid = new File('input.txt').readLines().collect { it.toList() }
    def rows = grid.size()
    def cols = grid[0].size()
    def energized = new HashSet<Tuple2<Integer, Integer>>()
    def visited = new HashSet<Tuple3<Integer, Integer, String>>()
    
    def queue = new LinkedList<Tuple3<Integer, Integer, String>>()
    queue.add(new Tuple3(0, 0, 'R'))

    while (!queue.isEmpty()) {
        def (row, col, dir) = queue.poll()

        if (row < 0 || row >= rows || col < 0 || col >= cols || visited.contains(new Tuple3(row, col, dir))) {
            continue
        }

        visited.add(new Tuple3(row, col, dir))
        energized.add(new Tuple2(row, col))

        def current = grid[row][col]
        
        switch (current) {
            case '.':
                switch (dir) {
                    case 'R': queue.add(new Tuple3(row, col + 1, 'R')); break
                    case 'L': queue.add(new Tuple3(row, col - 1, 'L')); break
                    case 'U': queue.add(new Tuple3(row - 1, col, 'U')); break
                    case 'D': queue.add(new Tuple3(row + 1, col, 'D')); break
                }
                break
            case '/':
                switch (dir) {
                    case 'R': queue.add(new Tuple3(row - 1, col, 'U')); break
                    case 'L': queue.add(new Tuple3(row + 1, col, 'D')); break
                    case 'U': queue.add(new Tuple3(row, col + 1, 'R')); break
                    case 'D': queue.add(new Tuple3(row, col - 1, 'L')); break
                }
                break
            case '\\':
                switch (dir) {
                    case 'R': queue.add(new Tuple3(row + 1, col, 'D')); break
                    case 'L': queue.add(new Tuple3(row - 1, col, 'U')); break
                    case 'U': queue.add(new Tuple3(row, col - 1, 'L')); break
                    case 'D': queue.add(new Tuple3(row, col + 1, 'R')); break
                }
                break
            case '|':
                switch (dir) {
                    case 'R':
                    case 'L':
                        queue.add(new Tuple3(row - 1, col, 'U'))
                        queue.add(new Tuple3(row + 1, col, 'D'))
                        break
                    case 'U': queue.add(new Tuple3(row - 1, col, 'U')); break
                    case 'D': queue.add(new Tuple3(row + 1, col, 'D')); break
                }
                break
            case '-':
                switch (dir) {
                    case 'U':
                    case 'D':
                        queue.add(new Tuple3(row, col - 1, 'L'))
                        queue.add(new Tuple3(row, col + 1, 'R'))
                        break
                    case 'R': queue.add(new Tuple3(row, col + 1, 'R')); break
                    case 'L': queue.add(new Tuple3(row, col - 1, 'L')); break
                }
                break
        }
    }
    println energized.size()
}

solve()
