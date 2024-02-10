
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

const lines = fs.readFileSync('input.txt', 'utf8').split('\n');
for (const line of lines) {
    if (line === "") {
        continue;
    }
    const cube = line.split(',').map(Number);
    cubes.set(cube.toString(), true);
    min = [Math.min(min[0], cube[0]), Math.min(min[1], cube[1]), Math.min(min[2], cube[2])];
    max = [Math.max(max[0], cube[0]), Math.max(max[1], cube[1]), Math.max(max[2], cube[2])];
}
min = [min[0] - 1, min[1] - 1, min[2] - 1];
max = [max[0] + 1, max[1] + 1, max[2] + 1];

let faces = 0;
let q = [min];
const seen = new Map();
seen.set(min.toString(), true);
while (q.length > 0) {
    const curr = q.shift();
    for (const delta of neighbors) {
        const next = [curr[0] + delta[0], curr[1] + delta[1], curr[2] + delta[2]];
        if (next[0] < min[0] || next[1] < min[1] || next[2] < min[2] || next[0] > max[0] || next[1] > max[1] || next[2] > max[2]) {
            continue;
        }
        if (cubes.has(next.toString())) {
            faces++;
        } else if (!seen.has(next.toString())) {
            seen.set(next.toString(), true);
            q.push(next);
        }
    }
}
console.log(faces);
