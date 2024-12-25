
const fs = require('fs');

const canReach = (grid) => {
  const n = grid.length;
  if (grid[0][0] || grid[n - 1][n - 1]) return false;
  const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
  const visited = Array(n).fill(null).map(() => Array(n).fill(false));
  const q = [[0, 0]];
  visited[0][0] = true;
  let head = 0;
  while (head < q.length) {
    const [x, y] = q[head++];
    if (x === n - 1 && y === n - 1) return true;
    for (const [dx, dy] of dirs) {
      const nx = x + dx;
      const ny = y + dy;
      if (nx >= 0 && ny >= 0 && nx < n && ny < n && !grid[ny][nx] && !visited[ny][nx]) {
        visited[ny][nx] = true;
        q.push([nx, ny]);
      }
    }
  }
  return false;
};

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const size = 71;
  const grid = Array(size).fill(null).map(() => Array(size).fill(false));
  const lines = data.trim().split('\n');
  for (const line of lines) {
    const [x, y] = line.split(',').map(Number);
    if (x >= 0 && x < size && y >= 0 && y < size) {
      grid[y][x] = true;
    }
    if (!canReach(grid)) {
      console.log(`${x},${y}`);
      return;
    }
  }
  console.log("No cutoff found");
});
