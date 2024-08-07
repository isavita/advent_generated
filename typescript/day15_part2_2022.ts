import * as fs from 'fs';

interface Sensor {
  pos: [number, number];
  beacon: [number, number];
  dist: number;
}

function readAll(path: string): string {
  return fs.readFileSync(path, 'utf8');
}

function abs(n: number): number {
  return n < 0 ? -n : n;
}

function manhattan(p: [number, number], q: [number, number]): number {
  return abs(p[0] - q[0]) + abs(p[1] - q[1]);
}

function distress(sensors: Sensor[], maxcoord: number): number {
  for (let x = 0; x <= maxcoord; x++) {
    for (let y = 0; y <= maxcoord; y++) {
      const p: [number, number] = [x, y];
      let detected = false;
      let skip = 0;
      for (const s of sensors) {
        if (manhattan(s.pos, p) <= s.dist) {
          detected = true;
          const dist = s.dist - abs(s.pos[0] - x);
          skip = Math.max(skip, dist + s.pos[1] - y);
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

function main() {
  const input = readAll('input.txt');
  const sensors: Sensor[] = [];
  for (const line of input.split('\n')) {
    const match = line.match(/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/);
    if (match) {
      const s: Sensor = {
        pos: [parseInt(match[1]), parseInt(match[2])],
        beacon: [parseInt(match[3]), parseInt(match[4])],
        dist: manhattan([parseInt(match[1]), parseInt(match[2])], [parseInt(match[3]), parseInt(match[4])]),
      };
      sensors.push(s);
    }
  }
  console.log(distress(sensors, 4000000));
}

main();