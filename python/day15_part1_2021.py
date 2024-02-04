
import heapq

class Position:
    def __init__(self, x, y, risk):
        self.x = x
        self.y = y
        self.risk = risk

    def __lt__(self, other):
        return self.risk < other.risk

def dijkstra(grid):
    pq = []
    heapq.heappush(pq, Position(0, 0, 0))

    rows = len(grid)
    cols = len(grid[0])
    dist = [[1 << 31 - 1 for _ in range(cols)] for _ in range(rows)]
    dist[0][0] = 0

    directions = [Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0)]

    while pq:
        curr = heapq.heappop(pq)
        if curr.x == rows - 1 and curr.y == cols - 1:
            return curr.risk
        for d in directions:
            nx, ny = curr.x + d.x, curr.y + d.y
            if 0 <= nx < rows and 0 <= ny < cols:
                next_risk = curr.risk + grid[nx][ny]
                if next_risk < dist[nx][ny]:
                    dist[nx][ny] = next_risk
                    heapq.heappush(pq, Position(nx, ny, next_risk))
    return -1

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        grid = []
        for line in file:
            row = [int(ch) for ch in line.strip()]
            grid.append(row)

    print(dijkstra(grid))
