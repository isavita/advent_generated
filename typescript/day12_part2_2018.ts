import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let initialState = '';
const rules: { [key: string]: string } = {};

input.forEach(line => {
  if (line.includes('initial state')) {
    initialState = line.split(': ')[1];
  } else if (line.includes('=>')) {
    const parts = line.split(' => ');
    rules[parts[0]] = parts[1];
  }
});

let state: { [key: number]: string } = {};
initialState.split('').forEach((c, i) => {
  if (c === '#') {
    state[i] = '#';
  }
});

let previousPattern = '';
let previousSum = 0;
let offset = 0;

for (let generation = 0; generation < 50000000000; generation++) {
  const newState: { [key: number]: string } = {};
  const [minPot, maxPot] = minMaxKeys(state);

  for (let i = minPot - 2; i <= maxPot + 2; i++) {
    let pattern = '';
    for (let j = i - 2; j <= i + 2; j++) {
      pattern += state[j] === '#' ? '#' : '.';
    }
    if (rules[pattern] === '#') {
      newState[i] = '#';
    }
  }
  state = newState;

  const [currentPattern, currentSum] = statePattern(state);
  if (currentPattern === previousPattern) {
    offset = currentSum - previousSum;
    const remainingGenerations = 50000000000 - generation - 1;
    const finalSum = currentSum + offset * remainingGenerations;
    console.log(finalSum);
    break;
  }
  previousPattern = currentPattern;
  previousSum = currentSum;
}

function minMaxKeys(m: { [key: number]: string }): [number, number] {
  let minKey = Infinity;
  let maxKey = -Infinity;
  for (const k in m) {
    const key = parseInt(k);
    if (key < minKey) minKey = key;
    if (key > maxKey) maxKey = key;
  }
  return [minKey, maxKey];
}

function statePattern(m: { [key: number]: string }): [string, number] {
  const [minPot, maxPot] = minMaxKeys(m);
  let pattern = '';
  let sum = 0;
  for (let i = minPot; i <= maxPot; i++) {
    if (m[i] === '#') {
      pattern += '#';
      sum += i;
    } else {
      pattern += '.';
    }
  }
  return [pattern, sum];
}