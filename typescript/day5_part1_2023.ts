import * as fs from 'fs';

interface RangeMap {
  srcStart: number;
  destStart: number;
  length: number;
}

function convertNumber(number: number, ranges: RangeMap[]): number {
  for (const r of ranges) {
    if (number >= r.srcStart && number < r.srcStart + r.length) {
      return r.destStart + (number - r.srcStart);
    }
  }
  return number;
}

const input = fs.readFileSync('input.txt', 'utf8');
const lines = input.split('\n');

const seeds: number[] = [];
const maps: RangeMap[][] = [];
let currentRanges: RangeMap[] = [];

for (const line of lines) {
  if (line.includes('map:')) {
    if (currentRanges.length > 0) {
      maps.push(currentRanges);
      currentRanges = [];
    }
  } else if (line.startsWith('seeds:')) {
    const seedStrs = line.slice(7).split(' ');
    for (const s of seedStrs) {
      seeds.push(parseInt(s, 10));
    }
  } else {
    const numbers = line.split(' ');
    if (numbers.length === 3) {
      const srcStart = parseInt(numbers[1], 10);
      const destStart = parseInt(numbers[0], 10);
      const length = parseInt(numbers[2], 10);

      currentRanges.push({ srcStart, destStart, length });
    }
  }
}
maps.push(currentRanges);

let minLocation = -1;
for (const seed of seeds) {
  let location = seed;
  for (const m of maps) {
    location = convertNumber(location, m);
  }

  if (minLocation === -1 || location < minLocation) {
    minLocation = location;
  }
}

console.log(minLocation);