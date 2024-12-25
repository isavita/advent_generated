
function canReach(grid: boolean[][]): boolean {
  const n = grid.length;
  if (grid[0][0] || grid[n - 1][n - 1]) {
    return false;
  }
  const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
  const visited = Array.from({ length: n }, () => Array(n).fill(false));
  const q: [number, number][] = [[0, 0]];
  visited[0][0] = true;
  let head = 0;
  while (head < q.length) {
    const [x, y] = q[head++];
    if (x === n - 1 && y === n - 1) {
      return true;
    }
    for (const [dx, dy] of dirs) {
      const nx = x + dx, ny = y + dy;
      if (nx >= 0 && ny >= 0 && nx < n && ny < n && !grid[ny][nx] && !visited[ny][nx]) {
        visited[ny][nx] = true;
        q.push([nx, ny]);
      }
    }
  }
  return false;
}

const fs = require('fs');
const lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const size = 71;
const grid = Array.from({ length: size }, () => Array(size).fill(false));
let i = 0;
for (const line of lines) {
  const [x, y] = line.split(',').map(Number);
  if (x >= 0 && x < size && y >= 0 && y < size) {
    grid[y][x] = true;
  }
  i++;
  if (!canReach(grid)) {
    console.log(`${x},${y}`);
    process.exit();
  }
}
console.log("No cutoff found");
