
function findPosition(mat: string[], ch: string): { i: number; j: number } {
  for (let i = 0; i < mat.length; i++) {
    for (let j = 0; j < mat[i].length; j++) {
      if (mat[i][j] === ch) {
        return { i, j };
      }
    }
  }
  return { i: -1, j: -1 };
}

function ok(mat: string[], st: { i: number; j: number }, seq: string): boolean {
  let curr = { i: st.i, j: st.j };
  for (let i = 0; i < seq.length; i++) {
    if (mat[curr.i][curr.j] === ' ') {
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
    if (
      curr.i < 0 ||
      curr.i >= mat.length ||
      curr.j < 0 ||
      curr.j >= mat[0].length
    ) {
      return false;
    }
  }
  return true;
}

function generateMoves(
  position: { i: number; j: number },
  objective: string,
  pad: string[]
): string {
  const objPos = findPosition(pad, objective);
  let ret = '';
  if (position.j > objPos.j) {
    ret += '<'.repeat(position.j - objPos.j);
  }
  if (position.i > objPos.i) {
    ret += '^'.repeat(position.i - objPos.i);
  }
  if (position.i < objPos.i) {
    ret += 'v'.repeat(objPos.i - position.i);
  }
  if (position.j < objPos.j) {
    ret += '>'.repeat(objPos.j - position.j);
  }
  if (!ok(pad, position, ret)) {
    ret = '';
    if (position.j < objPos.j) {
      ret += '>'.repeat(objPos.j - position.j);
    }
    if (position.i > objPos.i) {
      ret += '^'.repeat(position.i - objPos.i);
    }
    if (position.i < objPos.i) {
      ret += 'v'.repeat(objPos.i - position.i);
    }
    if (position.j > objPos.j) {
      ret += '<'.repeat(position.j - objPos.j);
    }
  }
  return ret;
}

function solve(
  code: string,
  robots: number,
  keyPad: string[],
  robotPad: string[],
  maxRobots: number,
  memo: Map<string, number>
): number {
  const memoKey = `${code}-${robots}`;
  if (memo.has(memoKey)) {
    return memo.get(memoKey)!;
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
    ret += solve(moves + 'A', robots - 1, keyPad, robotPad, maxRobots, memo);
  }
  memo.set(memoKey, ret);
  return ret;
}

const fs = require('fs');
const content = fs.readFileSync('input.txt', 'utf-8');
const maxRobots = 3;
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
  const memo = new Map<string, number>();
  const sv = solve(trimmedCode, maxRobots, keyPad, robotPad, maxRobots, memo);
  ret += sv * numericPart;
}
console.log(ret);