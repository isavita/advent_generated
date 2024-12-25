
const fs = require('fs');

interface Position {
  i: number;
  j: number;
}

interface CacheKey {
  pos: Position;
  ch: string;
  padType: string;
}

interface SolveKey {
  code: string;
  robots: number;
  maxRobots: number;
}

const positionCache = new Map<string, Position>();
const okCache = new Map<string, boolean>();
const moveCache = new Map<string, string>();
const solveCache = new Map<string, number>();

function findPosition(mat: string[], ch: string): Position {
  const key = ch + mat.join('');
  if (positionCache.has(key)) {
    return positionCache.get(key)!;
  }

  for (let i = 0; i < mat.length; i++) {
    for (let j = 0; j < mat[i].length; j++) {
      if (mat[i][j] === ch) {
        const pos = { i, j };
        positionCache.set(key, pos);
        return pos;
      }
    }
  }
  return { i: -1, j: -1 };
}

function ok(mat: string[], st: Position, seq: string): boolean {
  const key = `${st.i},${st.j},${seq},${mat.join('')}`;
  if (okCache.has(key)) {
    return okCache.get(key)!;
  }

  let curr = { ...st };
  for (let i = 0; i < seq.length; i++) {
    if (mat[curr.i][curr.j] === ' ') {
      okCache.set(key, false);
      return false;
    }

    const ch = seq[i];
    switch (ch) {
      case '^':
        curr.i--;
        break;
      case 'v':
        curr.i++;
        break;
      case '<':
        curr.j--;
        break;
      case '>':
        curr.j++;
        break;
    }

    if (curr.i < 0 || curr.i >= mat.length || curr.j < 0 || curr.j >= mat[0].length) {
      okCache.set(key, false);
      return false;
    }
  }

  okCache.set(key, true);
  return true;
}

function generateMoves(position: Position, objective: string, pad: string[]): string {
  const key: CacheKey = { pos: position, ch: objective, padType: pad.join('') };
  const keyString = JSON.stringify(key);
  if (moveCache.has(keyString)) {
    return moveCache.get(keyString)!;
  }

  const objPos = findPosition(pad, objective);

  let result = '';
  if (position.j > objPos.j) {
    result += '<'.repeat(position.j - objPos.j);
  }
  if (position.i > objPos.i) {
    result += '^'.repeat(position.i - objPos.i);
  }
  if (position.i < objPos.i) {
    result += 'v'.repeat(objPos.i - position.i);
  }
  if (position.j < objPos.j) {
    result += '>'.repeat(objPos.j - position.j);
  }

  if (!ok(pad, position, result)) {
    result = '';
    if (position.j < objPos.j) {
      result += '>'.repeat(objPos.j - position.j);
    }
    if (position.i > objPos.i) {
      result += '^'.repeat(position.i - objPos.i);
    }
    if (position.i < objPos.i) {
      result += 'v'.repeat(objPos.i - position.i);
    }
    if (position.j > objPos.j) {
      result += '<'.repeat(position.j - objPos.j);
    }
  }

  moveCache.set(keyString, result);
  return result;
}

function solve(code: string, robots: number, keyPad: string[], robotPad: string[], maxRobots: number): number {
  const key: SolveKey = { code, robots, maxRobots };
  const keyString = JSON.stringify(key);

  if (solveCache.has(keyString)) {
    return solveCache.get(keyString)!;
  }

  if (robots <= 0) {
    return code.length;
  }

  let ret = 0;
  let posi = 3,
    posj = 2;
  if (robots !== maxRobots) {
    posi = 0;
  }

  let moves = '';
  for (let i = 0; i < code.length; i++) {
    const ch = code[i];
    if (robots === maxRobots) {
      moves = generateMoves({ i: posi, j: posj }, ch, keyPad);
      const pos = findPosition(keyPad, ch);
      posi = pos.i;
      posj = pos.j;
    } else {
      moves = generateMoves({ i: posi, j: posj }, ch, robotPad);
      const pos = findPosition(robotPad, ch);
      posi = pos.i;
      posj = pos.j;
    }
    ret += solve(moves + 'A', robots - 1, keyPad, robotPad, maxRobots);
  }

  solveCache.set(keyString, ret);
  return ret;
}

const content = fs.readFileSync('input.txt', 'utf-8');

const maxRobots = 26;
const keyPad = ['789', '456', '123', ' 0A'];
const robotPad = [' ^A', '<v>'];

let ret = 0;
const codes = content.trim().split('\n');

for (const code of codes) {
  const trimmedCode = code.trim();
  if (trimmedCode === '') {
    continue;
  }

  let numericPart = 0;
  for (let i = 0; i < trimmedCode.length; i++) {
    if (trimmedCode[i] >= '0' && trimmedCode[i] <= '9') {
      numericPart = numericPart * 10 + parseInt(trimmedCode[i]);
    }
  }

  const sv = solve(trimmedCode, maxRobots, keyPad, robotPad, maxRobots);
  ret += sv * numericPart;
}

console.log(ret);
