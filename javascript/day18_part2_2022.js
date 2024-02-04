const fs = require('fs');

const cubes = new Map();
const neighbors = [
  [-1, 0, 0],
  [1, 0, 0],
  [0, -1, 0],
  [0, 1, 0],
  [0, 0, -1],
  [0, 0, 1],
];
let min = [Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER, Number.MAX_SAFE_INTEGER];
let max = [Number.MIN_SAFE_INTEGER, Number.MIN_SAFE_INTEGER, Number.MIN_SAFE_INTEGER];

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
for (const line of input) {
  if (line === '') {
    continue;
  }
  const cube = line.split(',').map(Number);
  cubes.set(cube.join(','), true);
  min = [
    Math.min(min[0], cube[0]),
    Math.min(min[1], cube[1]),
    Math.min(min[2], cube[2]),
  ];
  max = [
    Math.max(max[0], cube[0]),
    Math.max(max[1], cube[1]),
    Math.max(max[2], cube[2]),
  ];
}
min = min.map((val) => val - 1);
max = max.map((val) => val + 1);

let faces = 0;
const q = [min];
const seen = { [min.join(',')]: true };
while (q.length > 0) {
  const curr = q.shift();
  for (const delta of neighbors) {
    const next = curr.map((val, i) => val + delta[i]);
    if (
      next[0] < min[0] ||
      next[1] < min[1] ||
      next[2] < min[2] ||
      next[0] > max[0] ||
      next[1] > max[1] ||
      next[2] > max[2]
    ) {
      continue;
    }
    if (cubes.has(next.join(','))) {
      faces++;
    } else if (!seen[next.join(',')]) {
      seen[next.join(',')] = true;
      q.push(next);
    }
  }
}
console.log(faces);