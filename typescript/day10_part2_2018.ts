import * as fs from 'fs';

interface Star {
  x: number;
  y: number;
  vX: number;
  vY: number;
  next: Star | null;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const re = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/;

let head: Star = { x: 0, y: 0, vX: 0, vY: 0, next: null };
let tail: Star = head;

for (const line of input) {
  const split = re.exec(line);
  if (!split) continue;
  const star: Star = {
    x: parseInt(split[1]),
    y: parseInt(split[2]),
    vX: parseInt(split[3]),
    vY: parseInt(split[4]),
    next: null,
  };
  tail.next = star;
  tail = star;
}

let smallestT = 0;
let smallestArea = Infinity;

for (let t = 1; t < 100000; t++) {
  let maxX = -Infinity;
  let maxY = -Infinity;
  let minX = Infinity;
  let minY = Infinity;

  for (let temp = head.next; temp; temp = temp.next) {
    const x = temp.x + temp.vX * t;
    if (x > maxX) maxX = x;
    if (x < minX) minX = x;
    const y = temp.y + temp.vY * t;
    if (y > maxY) maxY = y;
    if (y < minY) minY = y;
  }

  const lenX = maxX - minX + 1;
  const lenY = maxY - minY + 1;
  const area = lenX + lenY;

  if (area < smallestArea) {
    smallestArea = area;
    smallestT = t;
  }
}

console.log(smallestT);

const t = smallestT;

let maxX = -Infinity;
let maxY = -Infinity;
let minX = Infinity;
let minY = Infinity;

for (let temp = head.next; temp; temp = temp.next) {
  temp.x += temp.vX * t;
  if (temp.x > maxX) maxX = temp.x;
  if (temp.x < minX) minX = temp.x;
  temp.y += temp.vY * t;
  if (temp.y > maxY) maxY = temp.y;
  if (temp.y < minY) minY = temp.y;
}

const mapper = Array(maxY - minY + 1)
  .fill(null)
  .map(() => Array(maxX - minX + 1).fill(false));

for (let temp = head.next; temp; temp = temp.next) {
  mapper[temp.y - minY][temp.x - minX] = true;
}

for (const row of mapper) {
  console.log(row.map((x) => (x ? '#' : '.')).join(''));
}