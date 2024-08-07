interface IntPriorityQueueItem {
  item: any;
  priority: number;
}

class IntPriorityQueue {
  private heap: IntPriorityQueueItem[];

  constructor() {
    this.heap = [];
  }

  push(item: IntPriorityQueueItem) {
    this.heap.push(item);
    this.heapifyUp(this.heap.length - 1);
  }

  pop(): IntPriorityQueueItem | undefined {
    if (this.heap.length === 0) return undefined;
    if (this.heap.length === 1) return this.heap.pop();

    const item = this.heap[0];
    this.heap[0] = this.heap.pop() as IntPriorityQueueItem;
    this.heapifyDown(0);

    return item;
  }

  private heapifyUp(index: number) {
    if (index === 0) return;
    const parentIndex = Math.floor((index - 1) / 2);
    if (this.heap[parentIndex].priority > this.heap[index].priority) {
      this.swap(parentIndex, index);
      this.heapifyUp(parentIndex);
    }
  }

  private heapifyDown(index: number) {
    const leftChildIndex = 2 * index + 1;
    const rightChildIndex = 2 * index + 2;
    let smallest = index;

    if (
      leftChildIndex < this.heap.length &&
      this.heap[leftChildIndex].priority < this.heap[smallest].priority
    ) {
      smallest = leftChildIndex;
    }

    if (
      rightChildIndex < this.heap.length &&
      this.heap[rightChildIndex].priority < this.heap[smallest].priority
    ) {
      smallest = rightChildIndex;
    }

    if (smallest !== index) {
      this.swap(smallest, index);
      this.heapifyDown(smallest);
    }
  }

  private swap(i: number, j: number) {
    const temp = this.heap[i];
    this.heap[i] = this.heap[j];
    this.heap[j] = temp;
  }
}

interface Coord {
  x: number;
  y: number;
}

interface Info {
  coord: Coord;
  dir: Coord;
  numStraight: number;
}

class Grid {
  private width: number;
  private height: number;
  private data: { [key: string]: number };

  constructor(width: number, height: number) {
    this.width = width;
    this.height = height;
    this.data = {};
  }

  public getGetWidth(): number {
    return this.width;
  }

  public getGetHeight(): number {
    return this.height;
  }

  public addData(coord: Coord, value: number) {
    this.data[`${coord.x},${coord.y}`] = value;
  }

  public getData(coord: Coord): number {
    return this.data[`${coord.x},${coord.y}`];
  }

  public neighbors4(coord: Coord): Coord[] {
    const neighbors: Coord[] = [];
    const directions: Coord[] = [
      { x: 0, y: -1 },
      { x: -1, y: 0 },
      { x: 0, y: 1 },
      { x: 1, y: 0 },
    ];

    for (const dir of directions) {
      const neighbor = { x: coord.x + dir.x, y: coord.y + dir.y };
      if (neighbor.x >= 0 && neighbor.x < this.width && neighbor.y >= 0 && neighbor.y < this.height) {
        neighbors.push(neighbor);
      }
    }

    return neighbors;
  }

  public dijkstra(start: Coord, goal: Coord): { [key: string]: Coord } {
    const frontier = new IntPriorityQueue();
    frontier.push({ item: start, priority: 0 });

    const cameFrom: { [key: string]: Coord } = {};
    const costSoFar: { [key: string]: number } = {};
    cameFrom[`${start.x},${start.y}`] = start;
    costSoFar[`${start.x},${start.y}`] = 0;

    while (true) {
      const current = frontier.pop();
      if (!current) break;

      const currentCoord = current.item as Coord;
      const currentCost = current.priority;

      if (currentCoord.x === goal.x && currentCoord.y === goal.y) break;

      for (const next of this.neighbors4(currentCoord)) {
        const newCost = currentCost + this.getData(next);
        if (!costSoFar[`${next.x},${next.y}`] || newCost < costSoFar[`${next.x},${next.y}`]) {
          costSoFar[`${next.x},${next.y}`] = newCost;
          frontier.push({ item: next, priority: newCost });
          cameFrom[`${next.x},${next.y}`] = currentCoord;
        }
      }
    }

    return cameFrom;
  }

  public aStarConstrained(start: Coord, goal: Coord, minStraight: number, maxStraight: number): number {
    const frontier = new IntPriorityQueue();
    frontier.push({ item: { coord: start, dir: { x: 0, y: 0 }, numStraight: 0 }, priority: 0 });

    const cameFrom: { [key: string]: Info } = {};
    const costSoFar: { [key: string]: number } = {};
    cameFrom[`${start.x},${start.y},${0},${0},${0}`] = { coord: start, dir: { x: 0, y: 0 }, numStraight: 0 };
    costSoFar[`${start.x},${start.y},${0},${0},${0}`] = 0;

    while (true) {
      const current = frontier.pop();
      if (!current) break;

      const currentInfo = current.item as Info;
      const currentCost = costSoFar[`${currentInfo.coord.x},${currentInfo.coord.y},${currentInfo.dir.x},${currentInfo.dir.y},${currentInfo.numStraight}`];

      if (currentInfo.coord.x === goal.x && currentInfo.coord.y === goal.y) return currentCost;

      for (const next of this.neighbors4(currentInfo.coord)) {
        const newDir = { x: next.x - currentInfo.coord.x, y: next.y - currentInfo.coord.y };
        let newNumStraight = 1;
        if (newDir.x === currentInfo.dir.x && newDir.y === currentInfo.dir.y) {
          newNumStraight += currentInfo.numStraight;
        }

        const nextInfo = { coord: next, dir: newDir, numStraight: newNumStraight };
        const newCost = currentCost + this.getData(next);

        if (
          !costSoFar[`${next.x},${next.y},${newDir.x},${newDir.y},${newNumStraight}`] ||
          newCost < costSoFar[`${next.x},${next.y},${newDir.x},${newDir.y},${newNumStraight}`]
        ) {
          if (
            (currentInfo.numStraight >= minStraight || newDir.x === currentInfo.dir.x || newDir.y === currentInfo.dir.y) &&
            newNumStraight <= maxStraight &&
            (newDir.x !== -currentInfo.dir.x || newDir.y !== -currentInfo.dir.y)
          ) {
            costSoFar[`${next.x},${next.y},${newDir.x},${newDir.y},${newNumStraight}`] = newCost;
            cameFrom[`${next.x},${next.y},${newDir.x},${newDir.y},${newNumStraight}`] = currentInfo;
            frontier.push({ item: nextInfo, priority: newCost + Math.abs(next.x - goal.x) + Math.abs(next.y - goal.y) });
          }
        }
      }
    }

    return -1;
  }
}

function solve(input: string[]): number {
  const grid = new Grid(input[0].length, input.length);
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      grid.addData({ x, y }, parseInt(input[y][x]));
    }
  }

  const start = { x: 0, y: 0 };
  const goal = { x: grid.getGetWidth() - 1, y: grid.getGetHeight() - 1 };
  return grid.aStarConstrained(start, goal, 4, 10);
}

import * as fs from 'fs';

function readFile(fileName: string): Promise<string[]> {
  return new Promise((resolve, reject) => {
    fs.readFile(fileName, 'utf8', (err, data) => {
      if (err) {
        reject(err);
      } else {
        resolve(data.split('\n'));
      }
    });
  });
}

readFile("input.txt").then((input) => console.log(solve(input)));