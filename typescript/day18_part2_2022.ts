import * as fs from 'fs';

type Pt3 = {
  x: number;
  y: number;
  z: number;
};

const neighbors: Pt3[] = [
  { x: -1, y: 0, z: 0 },
  { x: 1, y: 0, z: 0 },
  { x: 0, y: -1, z: 0 },
  { x: 0, y: 1, z: 0 },
  { x: 0, y: 0, z: -1 },
  { x: 0, y: 0, z: 1 },
];

const min = { x: Number.MAX_SAFE_INTEGER, y: Number.MAX_SAFE_INTEGER, z: Number.MAX_SAFE_INTEGER };
const max = { x: Number.MIN_SAFE_INTEGER, y: Number.MIN_SAFE_INTEGER, z: Number.MIN_SAFE_INTEGER };

const cubes: Record<string, true> = {};
const seen: Record<string, true> = {};

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

input.forEach(line => {
  if (line === '') return;
  const [x, y, z] = line.split(',').map(Number);
  const cube = { x, y, z };
  cubes[`${x},${y},${z}`] = true;
  min.x = Math.min(min.x, x);
  min.y = Math.min(min.y, y);
  min.z = Math.min(min.z, z);
  max.x = Math.max(max.x, x);
  max.y = Math.max(max.y, y);
  max.z = Math.max(max.z, z);
});

min.x--; min.y--; min.z--;
max.x++; max.y++; max.z++;

let faces = 0;
const q: Pt3[] = [min];
seen[`${min.x},${min.y},${min.z}`] = true;

while (q.length > 0) {
  const curr = q.shift()!;
  for (const delta of neighbors) {
    const next = { x: curr.x + delta.x, y: curr.y + delta.y, z: curr.z + delta.z };
    if (next.x < min.x || next.y < min.y || next.z < min.z || next.x > max.x || next.y > max.y || next.z > max.z) {
      continue;
    }
    const nextKey = `${next.x},${next.y},${next.z}`;
    if (cubes[nextKey]) {
      faces++;
    } else if (!seen[nextKey]) {
      seen[nextKey] = true;
      q.push(next);
    }
  }
}

console.log(faces);