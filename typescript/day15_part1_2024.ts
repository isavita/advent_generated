
import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').split('\n');
const grid: string[] = [];
let moves = '';
let readingMap = true;
for (const line of input) {
  if (readingMap) {
    if (line.includes('#')) {
      grid.push(line);
    } else {
      readingMap = false;
      moves += line;
    }
  } else {
    moves += line;
  }
}

const runes = grid.map((row) => row.split(''));
let robotR = 0,
  robotC = 0;
for (let r = 0; r < runes.length; r++) {
  for (let c = 0; c < runes[r].length; c++) {
    if (runes[r][c] === '@') {
      robotR = r;
      robotC = c;
    }
  }
}

const dirs: { [key: string]: [number, number] } = {
  '^': [-1, 0],
  v: [1, 0],
  '<': [0, -1],
  '>': [0, 1],
};

const pushBoxes = (
  runes: string[][],
  r: number,
  c: number,
  dr: number,
  dc: number
): boolean => {
  const nr = r + dr,
    nc = c + dc;
  if (nr < 0 || nr >= runes.length || nc < 0 || nc >= runes[0].length || runes[nr][nc] === '#') {
      return false;
  }
  if (runes[nr][nc] === 'O') {
    if (!pushBoxes(runes, nr, nc, dr, dc)) {
      return false;
    }
  }
  if (runes[nr][nc] === '.') {
    runes[nr][nc] = 'O';
    runes[r][c] = '.';
    return true;
  }
  return false;
};

for (const move of moves) {
  const d = dirs[move];
  const nr = robotR + d[0],
    nc = robotC + d[1];
    if (nr < 0 || nr >= runes.length || nc < 0 || nc >= runes[0].length) continue;
  if (runes[nr][nc] === '#') {
    continue;
  } else if (runes[nr][nc] === 'O') {
    if (!pushBoxes(runes, nr, nc, d[0], d[1])) {
      continue;
    }
  }
  if (runes[nr][nc] === '.' || runes[nr][nc] === 'O') {
    runes[robotR][robotC] = '.';
    runes[nr][nc] = '@';
    robotR = nr;
    robotC = nc;
  }
}

let sum = 0;
for (let r = 0; r < runes.length; r++) {
  for (let c = 0; c < runes[r].length; c++) {
    if (runes[r][c] === 'O') {
      sum += r * 100 + c;
    }
  }
}

console.log(sum);
