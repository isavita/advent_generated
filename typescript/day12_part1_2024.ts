
import * as fs from 'fs';

interface Point {
  x: number;
  y: number;
}

function solve(grid: string[]): number {
  const rows = grid.length;
  if (rows === 0) {
    return 0;
  }
  const cols = grid[0].length;

  const visited = Array.from({ length: rows }, () => Array(cols).fill(false));

  let totalPrice = 0;
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (!visited[r][c]) {
        const [area, perimeter] = calculateRegion(grid, r, c, visited);
        totalPrice += area * perimeter;
      }
    }
  }
  return totalPrice;
}

function calculateRegion(grid: string[], row: number, col: number, visited: boolean[][]): [number, number] {
  const rows = grid.length;
  const cols = grid[0].length;
  const char = grid[row][col];
  let area = 0;
  let perimeter = 0;

  const queue: Point[] = [{ x: row, y: col }];
  visited[row][col] = true;

  let head = 0;
  while (head < queue.length) {
    const p = queue[head++];
    area++;

    const neighbors: Point[] = [
      { x: p.x - 1, y: p.y },
      { x: p.x + 1, y: p.y },
      { x: p.x, y: p.y - 1 },
      { x: p.x, y: p.y + 1 },
    ];

    for (const n of neighbors) {
      if (n.x >= 0 && n.x < rows && n.y >= 0 && n.y < cols) {
        if (grid[n.x][n.y] !== char) {
          perimeter++;
        } else if (!visited[n.x][n.y]) {
          queue.push(n);
          visited[n.x][n.y] = true;
        }
      } else {
        perimeter++;
      }
    }
  }
  return [area, perimeter];
}

const grid = fs.readFileSync('input.txt', 'utf-8').split('\n').filter(line => line.trim() !== '');
const totalPrice = solve(grid);
console.log(totalPrice);
