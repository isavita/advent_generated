const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const solve = (input) => {
  const scanners = parseInput(input);

  const settled = [scanners[0]];
  settled[0].absoluteCoords = settled[0].relativeCoords;
  settled[0].fillAbsoluteCoordsMap();

  let undetermined = scanners.slice(1);

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

  const allBeacons = new Set();
  for (const s of settled) {
    for (const c of s.absoluteCoords) {
      allBeacons.add(c.join(','));
    }
  }

  return allBeacons.size;
};

const findAbsoluteCoordsForScanner = (undet, settled) => {
  for (const rotatedCoords of undet.rotations) {
    for (const set of settled) {
      for (const absCoord of set.absoluteCoords) {
        for (const relativeCoord of rotatedCoords) {
          const unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords);

          let matchingCount = 0;
          for (const ac of unsettledAbsoluteCoords) {
            if (set.absoluteCoordsMap.has(ac.join(','))) {
              matchingCount++;
            }
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
};

const makeAbsoluteCoordsList = (absolute, relative, relativeCoords) => {
  const diff = [
    absolute[0] - relative[0],
    absolute[1] - relative[1],
    absolute[2] - relative[2],
  ];

  return relativeCoords.map(c => [
    diff[0] + c[0],
    diff[1] + c[1],
    diff[2] + c[2],
  ]);
};

const parseInput = (input) => {
  return input.split('\n\n').map(rawScanner => {
    const lines = rawScanner.split('\n');
    const number = parseInt(lines[0].match(/\d+/)[0]);

    const coords = lines.slice(1).map(line => {
      const [x, y, z] = line.split(',').map(Number);
      return [x, y, z];
    });

    const sc = {
      number,
      x: 0,
      y: 0,
      z: 0,
      relativeCoords: coords,
      absoluteCoords: null,
      absoluteCoordsMap: new Set(),
      fillAbsoluteCoordsMap() {
        this.absoluteCoordsMap = new Set(this.absoluteCoords.map(ac => ac.join(',')));
      },
      fillRotations() {
        const posX = this.relativeCoords;
        const dir2 = [];
        const dir3 = [];
        const dir4 = [];
        const dir5 = [];
        const dir6 = [];
        for (const [x, y, z] of posX) {
          dir2.push([x, -y, -z]);
          dir3.push([x, -z, y]);
          dir4.push([-y, -z, x]);
          dir5.push([-x, -z, -y]);
          dir6.push([y, -z, -x]);
        }
        const sixRotations = [posX, dir2, dir3, dir4, dir5, dir6];

        const finalRotations = [];
        for (const rotation of sixRotations) {
          const r2 = [];
          const r3 = [];
          const r4 = [];
          for (const [x, y, z] of rotation) {
            r2.push([-y, x, z]);
            r3.push([-x, -y, z]);
            r4.push([y, -x, z]);
          }
          finalRotations.push(rotation, r2, r3, r4);
        }
        this.rotations = finalRotations;
      },
    };
    sc.fillRotations();
    return sc;
  });
};

const result = solve(input);
console.log(result);