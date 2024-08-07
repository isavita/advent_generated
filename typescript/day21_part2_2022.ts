import * as fs from 'fs';

interface Monkey {
  name: string;
  val: number | null;
  hasVal: boolean;
  left: Monkey | null;
  right: Monkey | null;
  op: string;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const index: { [key: string]: Monkey } = {};

function initMonkey(name: string) {
  if (!index[name]) {
    index[name] = { name, val: null, hasVal: false, left: null, right: null, op: '' };
  }
}

function parse() {
  for (const line of input) {
    const [name, expr] = line.split(': ');
    initMonkey(name);

    const num = parseInt(expr);
    if (!isNaN(num)) {
      index[name].val = num;
      index[name].hasVal = true;
      continue;
    }

    const [left, op, right] = expr.split(' ');
    initMonkey(left);
    initMonkey(right);

    index[name].left = index[left];
    index[name].op = op;
    index[name].right = index[right];
  }
}

function solve(monkey: Monkey): [number, boolean] {
  if (monkey.hasVal) {
    return [monkey.val as number, true];
  }

  if (monkey.left && monkey.right) {
    const [left, lOk] = solve(monkey.left);
    const [right, rOk] = solve(monkey.right);

    if (lOk && rOk) {
      switch (monkey.op) {
        case '+':
          return [left + right, true];
        case '-':
          return [left - right, true];
        case '*':
          return [left * right, true];
        case '/':
          return [left / right, true];
        case '==':
          return left === right ? [0, true] : [1, true];
      }
    }
  }
  return [0, false];
}

function expect(monkey: Monkey, x: number): number {
  if (monkey.name === 'humn') {
    return x;
  }

  const [left, lOk] = solve(monkey.left as Monkey);
  const [right, rOk] = solve(monkey.right as Monkey);

  if (!lOk) {
    switch (monkey.op) {
      case '+':
        return expect(monkey.left as Monkey, x - right);
      case '-':
        return expect(monkey.left as Monkey, x + right);
      case '*':
        return expect(monkey.left as Monkey, x / right);
      case '/':
        return expect(monkey.left as Monkey, x * right);
      case '==':
        return expect(monkey.left as Monkey, right);
    }
  }

  if (!rOk) {
    switch (monkey.op) {
      case '+':
        return expect(monkey.right as Monkey, x - left);
      case '-':
        return expect(monkey.right as Monkey, left - x);
      case '*':
        return expect(monkey.right as Monkey, x / left);
      case '/':
        return expect(monkey.right as Monkey, left / x);
      case '==':
        return expect(monkey.right as Monkey, left);
    }
  }
  throw new Error('impossible');
}

parse();
index['humn'].hasVal = false;
index['root'].op = '==';
console.log(expect(index['root'], 0));