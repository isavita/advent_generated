const fs = require('fs');

interface Mirror {
  rows: number[];
  cols: number[];
}

function parseInput(input: string[]): Mirror[] {
  const mirrors: Mirror[] = [];
  let mirrorStr: string[] = [];

  for (const line of input) {
    if (line === '') {
      mirrors.push(parseMirror(mirrorStr));
      mirrorStr = [];
    } else {
      mirrorStr.push(line);
    }
  }
  mirrors.push(parseMirror(mirrorStr));

  return mirrors;
}

function parseMirror(mirrorStr: string[]): Mirror {
  const mirror: Mirror = {
    rows: new Array(mirrorStr.length).fill(0),
    cols: new Array(mirrorStr[0].length).fill(0),
  };

  for (let y = 0; y < mirrorStr.length; y++) {
    for (let x = 0; x < mirrorStr[y].length; x++) {
      mirror.rows[y] <<= 1;
      mirror.cols[x] <<= 1;
      if (mirrorStr[y][x] === '#') {
        mirror.rows[y]++;
        mirror.cols[x]++;
      }
    }
  }

  return mirror;
}

function getMirrorAxis(lines: number[]): number {
  for (let i = 1; i < lines.length; i++) {
    let isMirror = true;

    for (let j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
      if (lines[i - 1 - j] !== lines[i + j]) {
        isMirror = false;
      }
    }

    if (isMirror) {
      return i;
    }
  }

  return 0;
}

function getMirrorAxisWithOneSmudge(lines: number[]): number {
  for (let i = 1; i < lines.length; i++) {
    let isMirror = true;
    let numSmudges = 0;

    for (let j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
      if (lines[i - 1 - j] !== lines[i + j]) {
        if (numSmudges > 0) {
          isMirror = false;
        } else {
          const dif = lines[i - 1 - j] ^ lines[i + j];
          const isOnlyOneSmudge = (dif & (dif - 1)) === 0;
          if (isOnlyOneSmudge) {
            numSmudges++;
          } else {
            isMirror = false;
          }
        }
      }
    }

    if (isMirror && numSmudges === 1) {
      return i;
    }
  }

  return 0;
}

function solve(input: string[]): number {
  const mirrors = parseInput(input);

  let res = 0;
  for (const mirror of mirrors) {
    res += getMirrorAxisWithOneSmudge(mirror.cols);
    res += getMirrorAxisWithOneSmudge(mirror.rows) * 100;
  }
  return res;
}

function readFile(fileName: string): string[] {
  const file = fs.readFileSync(fileName, 'utf8');
  return file.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));