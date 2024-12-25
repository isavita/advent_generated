
import * as fs from 'fs';

type Scanner = {
  number: number;
  x: number;
  y: number;
  z: number;
  relativeCoords: number[][];
  rotations: number[][][];
  absoluteCoords: number[][] | null;
  absoluteCoordsMap: Map<string, boolean>;
};

function solve(input: string): number {
  const scanners = parseInput(input);

  const settled: Scanner[] = [scanners[0]];
  settled[0].absoluteCoords = settled[0].relativeCoords;
  settled[0].absoluteCoordsMap = fillAbsoluteCoordsMap(settled[0].absoluteCoords);

  let undetermined = scanners.slice(1);

  while (undetermined.length > 0) {
    let i = 0;
    while (i < undetermined.length) {
      const undet = undetermined[i];
      const [maybeUpdated, ok] = findAbsoluteCoordsForScanner(undet, settled);
      if (ok) {
        settled.push(maybeUpdated);
        undetermined.splice(i, 1);
      } else {
        i++;
      }
    }
  }

  let furthest = 0;
  for (let i = 0; i < settled.length; i++) {
    for (let j = i + 1; j < settled.length; j++) {
      const s1 = settled[i];
      const s2 = settled[j];
      const manhattanDist =
        Math.abs(s1.x - s2.x) + Math.abs(s1.y - s2.y) + Math.abs(s1.z - s2.z);
      furthest = Math.max(furthest, manhattanDist);
    }
  }
  return furthest;
}

function fillAbsoluteCoordsMap(absoluteCoords: number[][]): Map<string, boolean> {
  const map = new Map<string, boolean>();
  for (const ac of absoluteCoords) {
    map.set(ac.toString(), true);
  }
  return map;
}

function fillRotations(s: Scanner): void {
  const posX = s.relativeCoords;
  const rotations: number[][][] = [];

  const rotateX = (coords: number[][]): number[][] =>
    coords.map(([x, y, z]) => [x, -z, y]);
  const rotateY = (coords: number[][]): number[][] =>
    coords.map(([x, y, z]) => [-z, y, x]);
  const rotateZ = (coords: number[][]): number[][] =>
    coords.map(([x, y, z]) => [-y, x, z]);

  let current = posX;
  for (let i = 0; i < 4; i++) {
    for (let j = 0; j < 4; j++) {
      for (let k = 0; k < 4; k++) {
        rotations.push(current);
        current = rotateZ(current);
      }
      current = rotateY(current);
    }
    current = rotateX(current);
  }

  const uniqueRotations = new Map<string, number[][]>();
  for (const rotation of rotations) {
    uniqueRotations.set(JSON.stringify(rotation), rotation);
  }

  s.rotations = Array.from(uniqueRotations.values());
}

function findAbsoluteCoordsForScanner(
  undet: Scanner,
  settled: Scanner[]
): [Scanner, boolean] {
  for (const rotatedCoords of undet.rotations) {
    for (const set of settled) {
      for (const absCoord of set.absoluteCoords!) {
        for (const relativeCoord of rotatedCoords) {
          const unsettledAbsoluteCoords = makeAbsoluteCoordsList(
            absCoord,
            relativeCoord,
            rotatedCoords
          );

          let matchingCount = 0;
          for (const ac of unsettledAbsoluteCoords) {
            if (set.absoluteCoordsMap.has(ac.toString())) {
              matchingCount++;
            }
          }

          if (matchingCount >= 12) {
            undet.relativeCoords = rotatedCoords;
            undet.absoluteCoords = unsettledAbsoluteCoords;
            undet.absoluteCoordsMap = fillAbsoluteCoordsMap(unsettledAbsoluteCoords);
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

function makeAbsoluteCoordsList(
  absolute: number[],
  relative: number[],
  relativeCoords: number[][]
): number[][] {
  const diff = [
    absolute[0] - relative[0],
    absolute[1] - relative[1],
    absolute[2] - relative[2],
  ];

  const absCoords: number[][] = [];
  for (const c of relativeCoords) {
    absCoords.push([diff[0] + c[0], diff[1] + c[1], diff[2] + c[2]]);
  }

  return absCoords;
}

function parseInput(input: string): Scanner[] {
  const ans: Scanner[] = [];
  const rawScanners = input.split('\n\n');
  for (const rawScanner of rawScanners) {
    const lines = rawScanner.split('\n');
    const number = parseInt(lines[0].match(/--- scanner (\d+) ---/)![1]);

    const coords: number[][] = [];
    for (let i = 1; i < lines.length; i++) {
      const [x, y, z] = lines[i].split(',').map(Number);
      coords.push([x, y, z]);
    }

    const sc: Scanner = {
      number,
      x: 0,
      y: 0,
      z: 0,
      relativeCoords: coords,
      rotations: [],
      absoluteCoords: null,
      absoluteCoordsMap: new Map<string, boolean>(),
    };
    fillRotations(sc);
    ans.push(sc);
  }

  return ans;
}

const file = fs.readFileSync('input.txt', 'utf-8');
const input = file.trim();
const result = solve(input);
console.log(result);
