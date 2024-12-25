
const fs = require('fs');

const size = 71;
const grid: boolean[][] = Array.from({ length: size }, () => Array(size).fill(false));
const input: string[] = fs.readFileSync('input.txt', 'utf-8').split('\n');

for (let i = 0; i < Math.min(1024, input.length); i++) {
  const line = input[i];
  const parts = line.split(',');
  const x = parseInt(parts[0]);
  const y = parseInt(parts[1]);
  if (x >= 0 && x < size && y >= 0 && y < size) {
    grid[y][x] = true;
  }
}

const dirs: number[][] = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const visited: boolean[][] = Array.from({ length: size }, () => Array(size).fill(false));

interface Pt {
  x: number;
  y: number;
}

const q: { pt: Pt; steps: number }[] = [{ pt: { x: 0, y: 0 }, steps: 0 }];
visited[0][0] = true;

let head = 0;
while (head < q.length) {
  const cur = q[head++];
  if (cur.pt.x === size - 1 && cur.pt.y === size - 1) {
    console.log(cur.steps);
    process.exit(0);
  }
  for (const d of dirs) {
    const nx = cur.pt.x + d[0];
    const ny = cur.pt.y + d[1];
    if (nx >= 0 && ny >= 0 && nx < size && ny < size && !grid[ny][nx] && !visited[ny][nx]) {
      visited[ny][nx] = true;
      q.push({ pt: { x: nx, y: ny }, steps: cur.steps + 1 });
    }
  }
}
console.log('No path');
