const fs = require('fs');

class Coord {
  constructor(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
  }
}

class Point {
  constructor(pos, vel) {
    this.pos = pos;
    this.vel = vel;
  }
}

function parseInput(input) {
  return input.map(line => {
    const [x1, y1, z1, x2, y2, z2] = line.match(/-?\d+(\.\d+)?/g).map(Number);
    return new Point(new Coord(x1, y1, z1), new Coord(x2, y2, z2));
  });
}

function isIntersecting2D(p1, p2) {
  const det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
  if (det === 0) {
    return [false, null, 0, 0];
  }
  const t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
  const t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;
  const coord = new Coord(p1.pos.x + p1.vel.x * t1, p1.pos.y + p1.vel.y * t1, 0);
  return [true, coord, t1, t2];
}

function solve(input, min, max) {
  const points = parseInput(input);

  let cnt = 0;
  for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < i; j++) {
      const [isIntersecting, coord, time1, time2] = isIntersecting2D(points[i], points[j]);
      if (isIntersecting && coord !== null) {
        const isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max;
        if (isInBound && time1 >= 0 && time2 >= 0) {
          cnt++;
        }
      }
    }
  }
  return cnt;
}

function readFile(fileName) {
  return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input, 200000000000000, 400000000000000));