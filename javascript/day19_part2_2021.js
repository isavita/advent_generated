const fs = require('fs');

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim();
  const result = solve(input);
  console.log(result);
}

function solve(input) {
  const scanners = parseInput(input);
  const settled = [scanners[0]];
  settled[0].absoluteCoords = settled[0].relativeCoords;
  settled[0].fillAbsoluteCoordsMap();
  const undetermined = scanners.slice(1);

  while (undetermined.length > 0) {
    for (let i = 0; i < undetermined.length; i++) {
      const [maybeUpdated, ok] = findAbsoluteCoordsForScanner(undetermined[i], settled);
      if (ok) {
        settled.push(maybeUpdated);
        undetermined.splice(i, 1);
        break;
      }
    }
  }

  let furthest = 0;
  for (let i = 0; i < settled.length; i++) {
    for (let j = 0; j < settled.length; j++) {
      if (i === j) continue;
      const manhattanDist = Math.abs(settled[i].x - settled[j].x) + Math.abs(settled[i].y - settled[j].y) + Math.abs(settled[i].z - settled[j].z);
      if (manhattanDist > furthest) furthest = manhattanDist;
    }
  }
  return furthest;
}

class Scanner {
  constructor(number, x, y, z, relativeCoords) {
    this.number = number;
    this.x = x;
    this.y = y;
    this.z = z;
    this.relativeCoords = relativeCoords;
    this.rotations = [];
    this.absoluteCoords = null;
    this.absoluteCoordsMap = new Map();
  }

  fillAbsoluteCoordsMap() {
    this.absoluteCoordsMap.clear();
    if (!this.absoluteCoords) throw new Error(`absolute coords not set for scanner ${this.number}`);
    for (const ac of this.absoluteCoords) this.absoluteCoordsMap.set(ac.join(','), true);
  }

  fillRotations() {
    const posX = this.relativeCoords;
    const dir2 = [], dir3 = [], dir4 = [], dir5 = [], dir6 = [];
    for (const [x, y, z] of posX) {
      dir2.push([x, -y, -z]);
      dir3.push([x, -z, y]);
      dir4.push([-y, -z, x]);
      dir5.push([-x, -z, -y]);
      dir6.push([y, -z, -x]);
    }
    const sixRotations = [posX, dir2, dir3, dir4, dir5, dir6];

    for (const rotation of sixRotations) {
      const r2 = [], r3 = [], r4 = [];
      for (const [x, y, z] of rotation) {
        r2.push([-y, x, z]);
        r3.push([-x, -y, z]);
        r4.push([y, -x, z]);
      }
      this.rotations.push(rotation, r2, r3, r4);
    }
  }
}

function findAbsoluteCoordsForScanner(undet, settled) {
  for (const rotatedCoords of undet.rotations) {
    for (const set of settled) {
      for (const absCoord of set.absoluteCoords) {
        for (const relativeCoord of rotatedCoords) {
          const unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords);
          let matchingCount = 0;
          for (const ac of unsettledAbsoluteCoords) {
            if (set.absoluteCoordsMap.has(ac.join(','))) matchingCount++;
          }
          if (matchingCount >= 12) {
            undet.relativeCoords = rotatedCoords;
            undet.absoluteCoords = unsettledAbsoluteCoords;
            undet.fillAbsoluteCoordsMap();
            undet.x = absCoord[0] - relativeCoord[0];
            undet.y = absCoord[1] - relativeCoord[1];
            undet.z = absCoord[2] - relativeCoord[2];
            return [undet, true];
          }
        }
      }
    }
  }
  return [undet, false];
}

function makeAbsoluteCoordsList(absolute, relative, relativeCoords) {
  const diff = [absolute[0] - relative[0], absolute[1] - relative[1], absolute[2] - relative[2]];
  return relativeCoords.map(([x, y, z]) => [diff[0] + x, diff[1] + y, diff[2] + z]);
}

function parseInput(input) {
  return input.split('\n\n').map(rawScanner => {
    const lines = rawScanner.split('\n');
    const number = parseInt(lines[0].match(/\d+/)[0]);
    const coords = lines.slice(1).map(line => line.split(',').map(Number));
    const sc = new Scanner(number, 0, 0, 0, coords);
    sc.fillRotations();
    return sc;
  });
}

main();