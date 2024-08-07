const fs = require('fs');

interface Row {
  springs: string;
  group: number[];
}

function parseInput(input: string[]): Row[] {
  return input.map(line => {
    const [springs, groupStr] = line.split(' ');
    const group = groupStr.split(',').map(Number);
    return { springs, group };
  });
}

function countArrangementsRecursive(row: Row, iSprings: number, iGroup: number, iContiguousDamaged: number, cache: { [key: string]: number }): number {
  if (iSprings === row.springs.length) {
    if (iGroup === row.group.length && iContiguousDamaged === 0) return 1;
    if (iGroup === row.group.length - 1 && iContiguousDamaged === row.group[iGroup]) return 1;
    return 0;
  }

  const cacheKey = `${iSprings},${iGroup},${iContiguousDamaged}`;
  if (cache[cacheKey] !== undefined) return cache[cacheKey];

  let res = 0;
  const char = row.springs[iSprings];
  if (char === '.' || char === '?') {
    if (iContiguousDamaged === 0) {
      res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
    } else if (iContiguousDamaged === row.group[iGroup]) {
      res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
    }
  }
  if (char === '#' || char === '?') {
    if (iGroup < row.group.length && iContiguousDamaged < row.group[iGroup]) {
      res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
    }
  }

  cache[cacheKey] = res;
  return res;
}

function countArrangements(row: Row): number {
  return countArrangementsRecursive(row, 0, 0, 0, {});
}

function unfoldRow(row: Row, unfoldingFactor: number): Row {
  const newRow: Row = { springs: row.springs, group: row.group };
  for (let i = 1; i < unfoldingFactor; i++) {
    newRow.springs += '?' + row.springs;
    newRow.group = newRow.group.concat(row.group);
  }
  return newRow;
}

function solve(input: string[]): number {
  const rows = parseInput(input);
  let res = 0;
  for (const row of rows) {
    res += countArrangements(row);
  }
  return res;
}

function readFile(fileName: string): string[] {
  const file = fs.readFileSync(fileName, 'utf8');
  return file.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));