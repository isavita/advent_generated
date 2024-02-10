const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map(line => line.trim());

const heightmap = input.map(line => line.split('').map(char => parseInt(char)));

let totalRiskLevel = 0;

for (let y = 0; y < heightmap.length; y++) {
  for (let x = 0; x < heightmap[y].length; x++) {
    if (isLowPoint(heightmap, x, y)) {
      totalRiskLevel += 1 + heightmap[y][x];
    }
  }
}

console.log(totalRiskLevel);

function isLowPoint(heightmap, x, y) {
  const height = heightmap[y][x];
  if (x > 0 && heightmap[y][x - 1] <= height) {
    return false;
  }
  if (x < heightmap[y].length - 1 && heightmap[y][x + 1] <= height) {
    return false;
  }
  if (y > 0 && heightmap[y - 1][x] <= height) {
    return false;
  }
  if (y < heightmap.length - 1 && heightmap[y + 1][x] <= height) {
    return false;
  }
  return true;
}