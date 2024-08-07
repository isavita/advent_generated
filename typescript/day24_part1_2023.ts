import * as fs from 'fs';

type Coord = {
  x: number;
  y: number;
  z: number;
};

type Point = {
  pos: Coord;
  vel: Coord;
};

function parseInput(input: string[]): Point[] {
  return input.map(line => {
    const [px, py, pz, vx, vy, vz] = line.match(/-?\d+/g)!.map(Number);
    return { pos: { x: px, y: py, z: pz }, vel: { x: vx, y: vy, z: vz } };
  });
}

function isIntersecting2D(p1: Point, p2: Point): [boolean, Coord, number, number] {
  const det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
  if (det === 0) return [false, { x: 0, y: 0, z: 0 }, 0, 0];

  const t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
  const t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;
  const coord = { x: p1.pos.x + p1.vel.x * t1, y: p1.pos.y + p1.vel.y * t1, z: 0 };

  return [true, coord, t1, t2];
}

function solve(input: string[], min: number, max: number): number {
  const points = parseInput(input);
  let cnt = 0;

  for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < i; j++) {
      const [isIntersecting, coord, time1, time2] = isIntersecting2D(points[i], points[j]);
      const isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max;

      if (isIntersecting && isInBound && time1 >= 0 && time2 >= 0) {
        cnt++;
      }
    }
  }

  return cnt;
}

function readFile(fileName: string): string[] {
  return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input, 200000000000000, 400000000000000));