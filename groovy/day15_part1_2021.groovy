import java.util.PriorityQueue

class Position implements Comparable<Position> {
    int x, y, risk;

    public Position(int x, int y, int risk) {
        this.x = x;
        this.y = y;
        this.risk = risk;
    }

    @Override
    public int compareTo(Position other) {
        return Integer.compare(this.risk, other.risk);
    }
}

def dijkstra(grid) {
    def pq = new PriorityQueue<Position>()
    pq.add(new Position(0, 0, 0))

    def rows = grid.size()
    def cols = grid[0].size()
    def dist = new int[rows][cols]
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            dist[i][j] = Integer.MAX_VALUE;
        }
    }
    dist[0][0] = 0

    def directions = [new Position(1, 0, 0), new Position(0, 1, 0), new Position(-1, 0, 0), new Position(0, -1, 0)]

    while (!pq.isEmpty()) {
        def curr = pq.poll()
        if (curr.x == rows - 1 && curr.y == cols - 1) {
            return curr.risk
        }
        for (dir in directions) {
            def nx = curr.x + dir.x
            def ny = curr.y + dir.y
            if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                def nextRisk = curr.risk + grid[nx][ny]
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk
                    pq.add(new Position(nx, ny, nextRisk))
                }
            }
        }
    }
    return -1
}

def grid = []
new File("input.txt").eachLine { line ->
    def row = []
    line.each { ch ->
        row.add(ch.toInteger())
    }
    grid.add(row)
}

println dijkstra(grid)