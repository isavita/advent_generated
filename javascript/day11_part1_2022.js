const fs = require('fs');

class Monkey {
  constructor() {
    this.items = [];
    this.operation = (old) => old;
    this.div = 0;
    this.next = [0, 0];
    this.inspections = 0;
  }
}

function parse(s) {
  const m = new Monkey();
  const lines = s.split('\n');
  m.items = lines[1].split(': ')[1].split(', ').map(Number);
  const [, op, val] = lines[2].split('= ')[1].split(' ');
  switch (op) {
    case '+':
      m.operation = val === 'old' ? (old) => old + old : (old) => old + Number(val);
      break;
    case '*':
      m.operation = val === 'old' ? (old) => old * old : (old) => old * Number(val);
      break;
  }
  m.div = Number(lines[3].split('by ')[1]);
  m.next = [Number(lines[4].split('monkey ')[1]), Number(lines[5].split('monkey ')[1])];
  return m;
}

function monkeyBusiness(monkeys, rounds, worry) {
  const div = monkeys.reduce((acc, m) => acc * m.div, 1);
  for (let i = 0; i < rounds; i++) {
    for (const m of monkeys) {
      while (m.items.length > 0) {
        m.inspections++;
        let item = m.operation(m.items.shift());
        if (!worry) {
          item = Math.floor(item / 3);
        } else {
          item %= div;
        }
        if (item % m.div === 0) {
          monkeys[m.next[0]].items.push(item);
        } else {
          monkeys[m.next[1]].items.push(item);
        }
      }
    }
  }
  const inspections = monkeys.map((m) => m.inspections).sort((a, b) => b - a);
  return inspections[0] * inspections[1];
}

const input = fs.readFileSync('input.txt', 'utf8');
const monkeys = input.split('\n\n').map(parse);
console.log(monkeyBusiness(monkeys, 20, false));