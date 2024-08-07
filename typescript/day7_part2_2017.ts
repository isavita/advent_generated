import * as fs from 'fs';

interface Program {
  weight: number;
  holds: string[];
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const programs: { [name: string]: Program } = {};

for (const line of input) {
  const matches = line.match(/[a-z]+|\d+/g);
  if (matches === null) {
    throw new Error(`Invalid input line: ${line}`);
  }
  const name = matches[0];
  const weight = parseInt(matches[1]);
  if (isNaN(weight)) {
    throw new Error(`Invalid weight in input line: ${line}`);
  }
  const program: Program = { weight, holds: matches.slice(2) };
  programs[name] = program;
}

function dfs(name: string): [number, boolean] {
  const program = programs[name];
  if (!program) {
    throw new Error(`Program not found: ${name}`);
  }
  let totalWeight = program.weight;

  const weights: { [weight: number]: number } = {};
  for (const child of program.holds) {
    const [weight, balanced] = dfs(child);
    if (!balanced) return [0, false];
    totalWeight += weight;
    weights[weight] = (weights[weight] || 0) + 1;
  }

  for (const w1 in weights) {
    for (const w2 in weights) {
      if (w1 !== w2 && weights[w1] < weights[w2]) {
        for (const child of program.holds) {
          if (dfs(child)[0] === parseInt(w1)) {
            console.log(programs[child].weight + (parseInt(w2) - parseInt(w1)));
            return [0, false];
          }
        }
      }
    }
  }
  return [totalWeight, true];
}

const root = 'dtacyn'; // Replace this with the root found in Part One
dfs(root);