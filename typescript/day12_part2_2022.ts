import * as fs from 'fs';

interface Point {
  x: number;
  y: number;
}

const grid: { [key: string]: string } = {};
let start: Point;
let end: Point;
let as: Point[] = [];

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const lines = data.split('\n');
  lines.forEach((line, y) => {
    for (let x = 0; x < line.length; x++) {
      const p: Point = { x, y };
      grid[`${p.x},${p.y}`] = line[x];
      if (line[x] === 'S') {
        start = p;
      } else if (line[x] === 'E') {
        end = p;
      } else if (line[x] === 'a') {
        as.push(p);
      }
    }
  });
  grid[`${start.x},${start.y}`] = 'a';
  grid[`${end.x},${end.y}`] = 'z';

  const dists = dijkstra(grid, end);

  let l = dists[`${start.x},${start.y}`];

  as.forEach((a) => {
    if (dists[`${a.x},${a.y}`]) {
      l = Math.min(l, dists[`${a.x},${a.y}`]);
    }
  });

  console.log(l);
});

interface Item {
  obj: Point;
  priority: number;
}

class PriorityQueue {
  private heap: Item[] = [];

  public push(item: Item) {
    this.heap.push(item);
    this.heapifyUp(this.heap.length - 1);
  }

  public pop(): Item | undefined {
    if (this.heap.length === 0) {
      return undefined;
    }
    const item = this.heap[0];
    this.heap[0] = this.heap[this.heap.length - 1];
    this.heap.pop();
    this.heapifyDown(0);
    return item;
  }

  private heapifyUp(index: number) {
    let parentIndex = Math.floor((index - 1) / 2);
    if (parentIndex < 0) {
      return;
    }
    if (this.heap[parentIndex].priority < this.heap[index].priority) {
      this.swap(parentIndex, index);
      this.heapifyUp(parentIndex);
    }
  }

  private heapifyDown(index: number) {
    let leftChildIndex = 2 * index + 1;
    let rightChildIndex = 2 * index + 2;
    let largest = index;
    if (
      leftChildIndex < this.heap.length &&
      this.heap[leftChildIndex].priority > this.heap[largest].priority
    ) {
      largest = leftChildIndex;
    }
    if (
      rightChildIndex < this.heap.length &&
      this.heap[rightChildIndex].priority > this.heap[largest].priority
    ) {
      largest = rightChildIndex;
    }
    if (largest !== index) {
      this.swap(largest, index);
      this.heapifyDown(largest);
    }
  }

  private swap(i: number, j: number) {
    const temp = this.heap[i];
    this.heap[i] = this.heap[j];
    this.heap[j] = temp;
  }
}

const neighbors4: Point[] = [
  { x: 0, y: 1 },
  { x: 0, y: -1 },
  { x: 1, y: 0 },
  { x: -1, y: 0 },
];

function dijkstra(grid: { [key: string]: string }, end: Point): { [key: string]: number } {
  const pq = new PriorityQueue();
  pq.push({ obj: end, priority: 0 });
  const dist: { [key: string]: number } = { [`${end.x},${end.y}`]: 0 };

  while (true) {
    const curr = pq.pop();
    if (!curr) {
      break;
    }
    for (const n of neighbors4) {
      const next: Point = { x: curr.obj.x + n.x, y: curr.obj.y + n.y };
      const nextKey = `${next.x},${next.y}`;
      if (!(nextKey in grid)) {
        continue;
      }
      if (grid[`${curr.obj.x},${curr.obj.y}`].charCodeAt(0) - grid[nextKey].charCodeAt(0) > 1) {
        continue;
      }
      const nextdist = dist[`${curr.obj.x},${curr.obj.y}`] + 1;
      if (!(nextKey in dist) || nextdist < dist[nextKey]) {
        dist[nextKey] = nextdist;
        pq.push({ obj: next, priority: nextdist });
      }
    }
  }

  return dist;
}