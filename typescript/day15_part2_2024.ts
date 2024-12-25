
const fs = require('fs');

const Up = { re: 0, im: -1 };
const Down = { re: 0, im: 1 };
const Left = { re: -1, im: 0 };
const Right = { re: 1, im: 0 };

function add(z1: { re: number; im: number }, z2: { re: number; im: number }): { re: number; im: number } {
  return { re: z1.re + z2.re, im: z1.im + z2.im };
}

function solve(input: string): number {
  const [m, steps] = parse(input);
  let robot: { re: number; im: number } = { re: 0, im: 0 };
  for (const k in m) {
    if (m[k] === '@') {
      robot = JSON.parse(k);
      break;
    }
  }

  for (const dir of steps) {
    if (tryToStep(m, robot, dir)) {
      robot = add(robot, dir);
    }
  }

  let sum = 0;
  for (const k in m) {
    if (m[k] === '[' || m[k] === 'O') {
      const { re, im } = JSON.parse(k);
      sum += re + 100 * im;
    }
  }
  return sum;
}

function tryToStep(m: Record<string, string>, pos: { re: number; im: number }, dir: { re: number; im: number }): boolean {
  const orig = { ...m };
  const posStr = JSON.stringify(pos);

  if (m[posStr] === '.') {
    return true;
  } else if (m[posStr] === 'O' || m[posStr] === '@') {
    if (tryToStep(m, add(pos, dir), dir)) {
      m[JSON.stringify(add(pos, dir))] = m[posStr];
      m[posStr] = '.';
      return true;
    }
  } else if (m[posStr] === ']') {
    if (tryToStep(m, add(pos, Left), dir)) {
      return true;
    }
  } else if (m[posStr] === '[') {
    if (dir.re === -1 && dir.im === 0) {
      if (tryToStep(m, add(pos, Left), dir)) {
        m[JSON.stringify(add(pos, Left))] = '[';
        m[posStr] = ']';
        m[JSON.stringify(add(pos, Right))] = '.';
        return true;
      }
    } else if (dir.re === 1 && dir.im === 0) {
      if (tryToStep(m, add(pos, add(Right, Right)), dir)) {
        m[posStr] = '.';
        m[JSON.stringify(add(pos, Right))] = '[';
        m[JSON.stringify(add(pos, add(Right, Right)))] = ']';
        return true;
      }
    } else {
      if (tryToStep(m, add(pos, dir), dir) && tryToStep(m, add(pos, add(Right, dir)), dir)) {
        m[posStr] = '.';
        m[JSON.stringify(add(pos, Right))] = '.';
        m[JSON.stringify(add(pos, dir))] = '[';
        m[JSON.stringify(add(pos, add(dir, Right)))] = ']';
        return true;
      }
    }
  }
  Object.assign(m, orig);
  return false;
}

function scaleUp(input: string): string {
  let s = input;
  s = s.replace(/#/g, '##');
  s = s.replace(/\./g, '..');
  s = s.replace(/O/g, '[]');
  s = s.replace(/@/g, '@.');
  return s;
}

function parse(input: string): [Record<string, string>, { re: number; im: number }[]] {
  const blocks = input.trim().split('\n\n');
  const lines = blocks[0].split('\n');
  const m: Record<string, string> = {};
  for (let y = 0; y < lines.length; y++) {
    for (let x = 0; x < lines[y].length; x++) {
      m[JSON.stringify({ re: x, im: y })] = lines[y][x];
    }
  }
  const steps: { re: number; im: number }[] = [];
  for (const ch of blocks[1].replace(/\n/g, '')) {
    switch (ch) {
      case '^':
        steps.push(Up);
        break;
      case '<':
        steps.push(Left);
        break;
      case '>':
        steps.push(Right);
        break;
      case 'v':
        steps.push(Down);
        break;
    }
  }
  return [m, steps];
}

const input = fs.readFileSync('input.txt', 'utf-8');
console.log(solve(input));
console.log(solve(scaleUp(input)));
