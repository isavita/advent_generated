const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.trim().split('\n');

const points = new Set();
const folds = [];
let readingPoints = true;

for (const line of lines) {
  if (line === '') {
    readingPoints = false;
    continue;
  }
  if (readingPoints) {
    const [x, y] = line.split(',').map(Number);
    points.add(`${x},${y}`);
  } else {
    const [axis, val] = line.split('=');
    folds.push([axis.includes('x') ? Number(val) : 0, axis.includes('y') ? Number(val) : 0]);
  }
}

for (let i = 0; i < folds.length; i++) {
  const [foldX, foldY] = folds[i];
  const newPoints = new Set();
  for (const point of points) {
    const [x, y] = point.split(',').map(Number);
    let newX = x;
    let newY = y;
    if (foldX !== 0 && x > foldX) {
      newX = foldX - (x - foldX);
    } else if (foldY !== 0 && y > foldY) {
      newY = foldY - (y - foldY);
    }
    newPoints.add(`${newX},${newY}`);
  }
  points.clear();
  for (const point of newPoints) {
    points.add(point);
  }
  if (i === 0) {
    console.log('Number of dots visible after first fold:', points.size);
  }
}

const [maxX, maxY] = Array.from(points).reduce(
  ([x, y], point) => {
    const [px, py] = point.split(',').map(Number);
    return [Math.max(x, px), Math.max(y, py)];
  },
  [0, 0]
);

const grid = Array.from({ length: maxY + 1 }, () => Array(maxX + 1).fill(' '));

for (const point of points) {
  const [x, y] = point.split(',').map(Number);
  grid[y][x] = '#';
}

console.log(grid.map(row => row.join('')).join('\n'));