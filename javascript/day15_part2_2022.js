const fs = require('fs');

class Sensor {
  constructor(pos, beacon, dist) {
    this.pos = pos;
    this.beacon = beacon;
    this.dist = dist;
  }
}

function readAll(path) {
  return fs.readFileSync(path, 'utf8');
}

function abs(n) {
  return n < 0 ? -n : n;
}

function manhattan(p, q) {
  return abs(p.x - q.x) + abs(p.y - q.y);
}

function distress(sensors, maxcoord) {
  for (let x = 0; x <= maxcoord; x++) {
    for (let y = 0; y <= maxcoord; y++) {
      let detected = false;
      let skip = 0;
      for (const s of sensors) {
        if (manhattan({ x, y }, s.pos) <= s.dist) {
          detected = true;
          const dist = s.dist - abs(s.pos.x - x);
          skip = Math.max(skip, dist + s.pos.y - y);
        }
      }
      if (!detected) {
        return x * 4000000 + y;
      }
      y += skip;
    }
  }
  return -1;
}

const input = readAll('input.txt');
const sensors = [];
for (const line of input.trim().split('\n')) {
  const [, sensorX, sensorY, beaconX, beaconY] = line.match(/x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/);
  const pos = { x: parseInt(sensorX), y: parseInt(sensorY) };
  const beacon = { x: parseInt(beaconX), y: parseInt(beaconY) };
  const dist = manhattan(pos, beacon);
  sensors.push(new Sensor(pos, beacon, dist));
}
console.log(distress(sensors, 4000000));