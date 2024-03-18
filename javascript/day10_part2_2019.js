const fs = require('fs');

class Asteroid {
  constructor(x, y, angle, dist) {
    this.x = x;
    this.y = y;
    this.angle = angle;
    this.dist = dist;
  }
}

function readAsteroids(filename) {
  const data = fs.readFileSync(filename, 'utf8');
  const asteroids = data.trim().split('\n').map(line => line.split('').map(char => char === '#'));
  return asteroids;
}

function vaporizeAsteroids(asteroids, station) {
  const targets = [];
  for (let y = 0; y < asteroids.length; y++) {
    for (let x = 0; x < asteroids[y].length; x++) {
      if (asteroids[y][x] && !(x === station[0] && y === station[1])) {
        const angle = Math.atan2(y - station[1], x - station[0]);
        let adjustedAngle = angle;
        if (adjustedAngle < -Math.PI / 2) {
          adjustedAngle += 2 * Math.PI; // Adjust angle for clockwise rotation
        }
        const dist = Math.hypot(x - station[0], y - station[1]);
        targets.push(new Asteroid(x, y, adjustedAngle, dist));
      }
    }
  }

  targets.sort((a, b) => {
    if (a.angle === b.angle) {
      return a.dist - b.dist;
    }
    return a.angle - b.angle;
  });

  const vaporized = [];
  while (targets.length > 0) {
    let lastAngle = -Infinity;
    for (let i = 0; i < targets.length; ) {
      if (targets[i].angle !== lastAngle) {
        vaporized.push(targets[i]);
        lastAngle = targets[i].angle;
        targets.splice(i, 1);
      } else {
        i++;
      }
    }
  }
  return vaporized;
}

function findBestAsteroidLocation(asteroids) {
  let bestLocation = null;
  let maxCount = 0;
  for (let y = 0; y < asteroids.length; y++) {
    for (let x = 0; x < asteroids[y].length; x++) {
      if (asteroids[y][x]) {
        const count = countVisibleAsteroids(asteroids, x, y);
        if (count > maxCount) {
          maxCount = count;
          bestLocation = [x, y];
        }
      }
    }
  }
  return [bestLocation, maxCount];
}

function countVisibleAsteroids(asteroids, x, y) {
  const angles = new Set();
  for (let otherY = 0; otherY < asteroids.length; otherY++) {
    for (let otherX = 0; otherX < asteroids[otherY].length; otherX++) {
      if (asteroids[otherY][otherX] && !(otherX === x && otherY === y)) {
        const angle = Math.atan2(otherY - y, otherX - x);
        angles.add(angle);
      }
    }
  }
  return angles.size;
}

const asteroids = readAsteroids('input.txt');
const [station, _] = findBestAsteroidLocation(asteroids);
const vaporized = vaporizeAsteroids(asteroids, station);
if (vaporized.length >= 200) {
  const result = vaporized[199].x * 100 + vaporized[199].y;
  console.log(result);
} else {
  console.log('Less than 200 asteroids were vaporized.');
}