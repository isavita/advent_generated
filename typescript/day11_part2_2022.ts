const fs = require('fs');

class Monkey {
  constructor() {
    this.items = [];
    this.operation = null;
    this.div = 0;
    this.next = [0, 0];
    this.inspections = 0;
  }
}

function parse(s) {
  const m = new Monkey();
  const lines = s.split('\n');
  
  lines[1].split(': ')[1].split(', ').forEach(item => {
    m.items.push(parseInt(item));
  });

  const f = lines[2].split('= ')[1].split(' ');
  switch (f[1]) {
    case '+':
      m.operation = f[2] === 'old' ? old => old + old : old => old + parseInt(f[2]);
      break;
    case '*':
      m.operation = f[2] === 'old' ? old => old * old : old => old * parseInt(f[2]);
      break;
  }

  m.div = parseInt(lines[3].match(/(\d+)/)[0]);
  m.next[0] = parseInt(lines[4].match(/(\d+)/)[0]);
  m.next[1] = parseInt(lines[5].match(/(\d+)/)[0]);

  return m;
}

function monkeyBusiness(monkeys, rounds, worry) {
  let div = 1;
  monkeys.forEach(m => div *= m.div);

  for (let i = 0; i < rounds; i++) {
    monkeys.forEach(m => {
      while (m.items.length > 0) {
        m.inspections++;
        let item = m.operation(m.items[0]);
        if (worry) {
          item %= div;
        } else {
          item /= 3;
        }
        if (item % m.div === 0) {
          monkeys[m.next[0]].items.push(item);
        } else {
          monkeys[m.next[1]].items.push(item);
        }
        m.items.shift();
      }
    });
  }

  const inspections = monkeys.map(m => m.inspections);
  inspections.sort((a, b) => b - a);
  return inspections[0] * inspections[1];
}

const input = fs.readFileSync('input.txt', 'utf8');
const monkeys = input.split('\n\n').map(m => parse(m));
console.log(monkeyBusiness(monkeys, 10000, true));