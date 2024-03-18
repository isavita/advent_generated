const fs = require('fs');

function readAll(path) {
  return fs.readFileSync(path, 'utf8').trim();
}

function move(st, steps) {
  const stacks = st.map(s => s.slice().reverse());
  for (const step of steps) {
    const [, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/);
    for (let i = 0; i < +n; i++) {
      stacks[+to - 1].push(stacks[+from - 1].pop());
    }
  }
  return stacks.map(s => s[s.length - 1]).join('');
}

const [input, steps] = readAll('input.txt').split('\n\n');
const stacks = [];
for (const line of input.split('\n')) {
  for (let i = 1; i < line.length; i += 4) {
    if (line[i] >= 'A' && line[i] <= 'Z') {
      stacks[Math.floor((i - 1) / 4)] = (stacks[Math.floor((i - 1) / 4)] || []).concat(line[i]);
    }
  }
}
console.log(move(stacks, steps.split('\n')));