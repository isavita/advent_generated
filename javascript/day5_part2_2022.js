const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n\n');
const inputLines = input[0].split('\n');
const stacks = new Array(Math.floor((inputLines[0].length + 1) / 4));

inputLines.forEach(line => {
  [...line].forEach((b, i) => {
    if (b >= 'A' && b <= 'Z') {
      stacks[Math.floor((i - 1) / 4)] = (stacks[Math.floor((i - 1) / 4)] || '') + b;
    }
  });
});

const steps = input[1].split('\n');

console.log(move(stacks, steps));

function move(st, steps) {
  const stacks = new Array(st.length);

  st.forEach((stack, i) => {
    stacks[i] = stack.split('').reverse().join('');
  });

  steps.forEach(step => {
    const [, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/).map(Number);
    stacks[to - 1] += stacks[from - 1].slice(-n);
    stacks[from - 1] = stacks[from - 1].slice(0, -n);
  });

  return stacks.map(stack => stack.slice(-1)).join('');
}