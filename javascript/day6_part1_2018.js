const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const points = input.map(line => line.split(', ').map(Number));

const maxX = Math.max(...points.map(point => point[0]));
const maxY = Math.max(...points.map(point => point[1]));

const grid = Array.from({ length: maxX + 2 }, () => Array(maxY + 2).fill(0));
const areas = Array(points.length).fill(0);
const infinite = Array(points.length).fill(false);

for (let i = 0; i < grid.length; i++) {
  for (let j = 0; j < grid[i].length; j++) {
    let minDist = maxX + maxY;
    for (let k = 0; k < points.length; k++) {
      const dist = Math.abs(points[k][0] - i) + Math.abs(points[k][1] - j);
      if (dist < minDist) {
        minDist = dist;
        grid[i][j] = k;
      } else if (dist === minDist) {
        grid[i][j] = -1;
      }
    }
    if (grid[i][j] !== -1) {
      if (i === 0 || j === 0 || i === maxX + 1 || j === maxY + 1) {
        infinite[grid[i][j]] = true;
      }
      areas[grid[i][j]]++;
    }
  }
}

let maxArea = 0;
for (let i = 0; i < areas.length; i++) {
  if (!infinite[i] && areas[i] > maxArea) {
    maxArea = areas[i];
  }
}

console.log(maxArea);