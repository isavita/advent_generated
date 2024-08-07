const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let initialState = '';
const rules: { [key: string]: string } = {};

for (const line of input) {
  if (line.includes('initial state')) {
    initialState = line.split(': ')[1];
  } else if (line.includes('=>')) {
    const [pattern, result] = line.split(' => ');
    rules[pattern] = result;
  }
}

let state: { [key: number]: string } = {};
for (let i = 0; i < initialState.length; i++) {
  if (initialState[i] === '#') {
    state[i] = '#';
  }
}

for (let generation = 0; generation < 20; generation++) {
  const newState: { [key: number]: string } = {};
  let minPot = Infinity;
  let maxPot = -Infinity;
  for (const pot in state) {
    if (parseInt(pot) < minPot) {
      minPot = parseInt(pot);
    }
    if (parseInt(pot) > maxPot) {
      maxPot = parseInt(pot);
    }
  }
  for (let i = minPot - 2; i <= maxPot + 2; i++) {
    let pattern = '';
    for (let j = i - 2; j <= i + 2; j++) {
      pattern += state[j] || '.';
    }
    if (rules[pattern] === '#') {
      newState[i] = '#';
    }
  }
  state = { ...newState }; // Use the spread operator to create a new object
}

let sum = 0;
for (const pot in state) {
  sum += parseInt(pot);
}

console.log(sum);