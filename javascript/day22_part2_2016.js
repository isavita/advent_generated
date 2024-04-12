const fs = require('fs');

class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  toString() {
    return `${this.x}-${this.y}`;
  }

  equals(other) {
    return this.x === other.x && this.y === other.y;
  }

  add(other) {
    return new Point(this.x + other.x, this.y + other.y);
  }

  static fromString(str) {
    const [x, y] = str.split('-').map(Number);
    return new Point(x, y);
  }
}

const Neighbors4 = [new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)];
const re = /-x(\d+)-y(\d+)/;

class Node {
  constructor(used, avail) {
    this.used = used;
    this.avail = avail;
  }
}

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
    return this.items.shift();
  }

  isEmpty() {
    return this.items.length === 0;
  }
}

function readAll() {
  return fs.readFileSync('input.txt', 'utf8').trim();
}

function minMoves(nodes) {
  const [w, h] = dim(nodes);
  let goal = new Point(w, 0);
  let hole = findHole(nodes);
  let sum = 0;

  while (!goal.equals(new Point(0, 0))) {
    let next = goal.add(new Point(-1, 0));
    let m = moves(nodes, goal, hole, next);
    sum += m;
    hole = next;
    m = moves(nodes, goal, goal, hole);
    sum += m;
    [goal, hole] = [hole, goal];
  }

  return sum;
}

function findHole(nodes) {
  for (let p in nodes) {
    if (nodes[p].used === 0) {
      return Point.fromString(p);
    }
  }
  throw new Error("no hole");
}

function moves(nodes, goal, from, to) {
  const [w, h] = dim(nodes);
  const depth = { [from.toString()]: 0 };
  const pq = new PriorityQueue();
  pq.push(new Item(from, 0));

  while (!pq.isEmpty()) {
    const p = pq.pop().obj;
    if (p.equals(to)) {
      return depth[p.toString()];
    }
    const currDepth = depth[p.toString()] + 1;
    Neighbors4.forEach(n => {
      const next = p.add(n);
      if (next.x < 0 || next.y < 0 || next.x > w || next.y > h || nodes[next.toString()].used > 400 || next.equals(goal)) {
        return;
      }
      const nextKey = next.toString();
      if (!(nextKey in depth) || currDepth < depth[nextKey]) {
        depth[nextKey] = currDepth;
        pq.push(new Item(next, -currDepth));
      }
    });
  }

  throw new Error("no possible path");
}

function dim(nodes) {
  let w = 0, h = 0;
  for (let key in nodes) {
    const p = Point.fromString(key);
    if (p.x > w) w = p.x;
    if (p.y > h) h = p.y;
  }
  return [w, h];
}

function main() {
  const nodes = {};
  const input = readAll().split('\n').slice(2);
  input.forEach(line => {
    const f = line.split(/\s+/);
    const matches = re.exec(f[0]);
    const p = new Point(parseInt(matches[1]), parseInt(matches[2])).toString();
    const used = parseInt(f[2].slice(0, -1));
    const avail = parseInt(f[3].slice(0, -1));
    nodes[p] = new Node(used, avail);
  });
  console.log(minMoves(nodes));
}

main();