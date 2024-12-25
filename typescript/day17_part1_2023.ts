
type Coord = {
  x: number;
  y: number;
};

type Grid = {
  width: number;
  height: number;
  data: number[][];
};

type Info = {
  coord: Coord;
  dir: Coord;
  numStraight: number;
};

type PriorityQueueItem = {
  item: Info;
  priority: number;
};

class PriorityQueue {
  private items: PriorityQueueItem[] = [];

  enqueue(item: Info, priority: number): void {
    const queueItem: PriorityQueueItem = { item, priority };
    let contain = false;

    for (let i = 0; i < this.items.length; i++) {
      if (this.items[i].priority > queueItem.priority) {
        this.items.splice(i, 0, queueItem);
        contain = true;
        break;
      }
    }

    if (!contain) {
      this.items.push(queueItem);
    }
  }

  dequeue(): Info | undefined {
    if (this.isEmpty()) {
      return undefined;
    }
    return this.items.shift()?.item;
  }

  isEmpty(): boolean {
    return this.items.length === 0;
  }
}

const North: Coord = { x: 0, y: -1 };
const West: Coord = { x: -1, y: 0 };
const South: Coord = { x: 0, y: 1 };
const East: Coord = { x: 1, y: 0 };

function add(c1: Coord, c2: Coord): Coord {
  return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function subtract(c1: Coord, c2: Coord): Coord {
  return { x: c1.x - c2.x, y: c1.y - c2.y };
}

function opposite(c: Coord): Coord {
  return { x: -c.x, y: -c.y };
}

function isInBounds(coord: Coord, grid: Grid): boolean {
  return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}

function abs(x: number): number {
  return x < 0 ? -x : x;
}

function buildGrid(input: string[]): Grid {
  const height = input.length;
  const width = input[0].length;
  const data: number[][] = [];

  for (let y = 0; y < height; y++) {
    data[y] = [];
    for (let x = 0; x < width; x++) {
      data[y][x] = parseInt(input[y][x]);
    }
  }

  return { width, height, data };
}

function neighbors4(coord: Coord, grid: Grid): Coord[] {
  const neighbors: Coord[] = [];
  const directions: Coord[] = [North, West, South, East];

  for (const dir of directions) {
    const neighbor = add(coord, dir);
    if (isInBounds(neighbor, grid)) {
      neighbors.push(neighbor);
    }
  }

  return neighbors;
}

function heuristic(c1: Coord, c2: Coord): number {
  return abs(c1.x - c2.x) + abs(c1.y - c2.y);
}

function aStarConstrained(grid: Grid, start: Coord, goal: Coord, minStraight: number, maxStraight: number): number {
  const startInfo: Info = { coord: start, dir: { x: 0, y: 0 }, numStraight: 0 };

  const frontier = new PriorityQueue();
  frontier.enqueue(startInfo, 0);

  const cameFrom: Map<string, Info> = new Map();
  const costSoFar: Map<string, number> = new Map();
  cameFrom.set(JSON.stringify(startInfo), startInfo);
  costSoFar.set(JSON.stringify(startInfo), 0);

  while (!frontier.isEmpty()) {
    const current = frontier.dequeue()!;
    const currentCost = costSoFar.get(JSON.stringify(current))!;

    if (current.coord.x === goal.x && current.coord.y === goal.y) {
      return currentCost;
    }

    for (const next of neighbors4(current.coord, grid)) {
      const newDir = subtract(next, current.coord);
      let newNumStraight = 1;
      if (newDir.x === current.dir.x && newDir.y === current.dir.y) {
        newNumStraight += current.numStraight;
      }

      const nextInfo: Info = { coord: next, dir: newDir, numStraight: newNumStraight };
      const newCost = currentCost + grid.data[next.y][next.x];
      const actualCost = costSoFar.get(JSON.stringify(nextInfo));

      const isLowerCost = actualCost === undefined || newCost < actualCost;
      const isValidStraight = (current.numStraight >= minStraight || (newDir.x === current.dir.x && newDir.y === current.dir.y) || (current.coord.x === start.x && current.coord.y === start.y)) && newNumStraight <= maxStraight;
      const isNotOppositeDirection = !(newDir.x === -current.dir.x && newDir.y === -current.dir.y);

      if (isLowerCost && isValidStraight && isNotOppositeDirection) {
        costSoFar.set(JSON.stringify(nextInfo), newCost);
        cameFrom.set(JSON.stringify(nextInfo), current);

        const priority = newCost + heuristic(next, goal);
        frontier.enqueue(nextInfo, priority);
      }
    }
  }

  return -1;
}

function solve(input: string[]): number {
  const grid = buildGrid(input);
  const start: Coord = { x: 0, y: 0 };
  const goal: Coord = { x: grid.width - 1, y: grid.height - 1 };
  return aStarConstrained(grid, start, goal, 0, 3);
}

function readFile(fileName: string): string[] {
  const fs = require('fs');
  const data = fs.readFileSync(fileName, 'utf-8');
  return data.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));
