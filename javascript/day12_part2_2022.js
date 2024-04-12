const fs = require('fs');

class Item {
  constructor(obj, priority) {
    this.obj = obj;
    this.priority = priority;
  }
}

class PriorityQueue {
  constructor() {
    this.items = [];
  }

  push(item) {
    this.items.push(item);
    this.items.sort((a, b) => b.priority - a.priority);
  }

  pop() {
    return this.items.pop();
  }

  isEmpty() {
    return this.items.length === 0;
  }
}

const Neighbors4 = [
  { x: 0, y: 1 },
  { x: 0, y: -1 },
  { x: 1, y: 0 },
  { x: -1, y: 0 },
];

function djikstra(grid, end) {
  const pq = new PriorityQueue();
  pq.push(new Item(end, 0));
  const dist = { [`${end.x},${end.y}`]: 0 };

  while (!pq.isEmpty()) {
    const curr = pq.pop().obj;
    for (const n of Neighbors4) {
      const next = { x: curr.x + n.x, y: curr.y + n.y };
      if (!grid.hasOwnProperty(`${next.x},${next.y}`)) {
        continue;
      }
      if (grid[`${curr.x},${curr.y}`].charCodeAt(0) - grid[`${next.x},${next.y}`].charCodeAt(0) > 1) {
        continue;
      }
      const nextDist = dist[`${curr.x},${curr.y}`] + 1;
      if (!dist.hasOwnProperty(`${next.x},${next.y}`) || nextDist < dist[`${next.x},${next.y}`]) {
        dist[`${next.x},${next.y}`] = nextDist;
        pq.push(new Item(next, nextDist));
      }
    }
  }
  return dist;
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf8');
  const lines = input.trim().split('\n');

  const grid = {};
  let start, end;
  const as = [];

  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[y].length; x++) {
      const char = lines[y][x];
      grid[`${x},${y}`] = char;
      if (char === 'S') {
        start = { x, y };
      } else if (char === 'E') {
        end = { x, y };
      } else if (char === 'a') {
        as.push({ x, y });
      }
    }
  }

  grid[`${start.x},${start.y}`] = 'a';
  grid[`${end.x},${end.y}`] = 'z';

  const dists = djikstra(grid, end);

  let l = dists[`${start.x},${start.y}`];

  for (const a of as) {
    if (dists.hasOwnProperty(`${a.x},${a.y}`)) {
      l = Math.min(l, dists[`${a.x},${a.y}`]);
    }
  }

  console.log(l);
}

main();