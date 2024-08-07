const fs = require('fs');

interface RangeMap {
  srcStart: number;
  destStart: number;
  length: number;
}

function reverseConvertNumber(number: number, ranges: RangeMap[]): number {
  for (let i = ranges.length - 1; i >= 0; i--) {
    const r = ranges[i];
    if (number >= r.destStart && number < r.destStart + r.length) {
      return r.srcStart + (number - r.destStart);
    }
  }
  return number;
}

function isInSeedRanges(number: number, ranges: [number, number][]): boolean {
  for (const r of ranges) {
    if (number >= r[0] && number < r[0] + r[1]) {
      return true;
    }
  }
  return false;
}

function main() {
  const fileContent = fs.readFileSync('input.txt', 'utf8');
  const lines = fileContent.split('\n');
  const seedRanges: [number, number][] = [];
  let currentRanges: RangeMap[] = [];
  const maps: RangeMap[][] = [];

  for (const line of lines) {
    if (line.includes('map:')) {
      if (currentRanges.length > 0) {
        maps.push(currentRanges);
        currentRanges = [];
      }
    } else if (line.startsWith('seeds:')) {
      const seedStrs = line.slice(7).split(' ');
      for (let i = 0; i < seedStrs.length; i += 2) {
        const start = parseInt(seedStrs[i], 10);
        const length = parseInt(seedStrs[i + 1], 10);
        seedRanges.push([start, length]);
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
  if (currentRanges.length > 0) {
    maps.push(currentRanges);
  }

  let location = 0;
  while (true) {
    let seed = location;
    for (let i = maps.length - 1; i >= 0; i--) {
      seed = reverseConvertNumber(seed, maps[i]);
    }

    if (isInSeedRanges(seed, seedRanges)) {
      console.log(location);
      break;
    }
    location++;
  }
}

main();