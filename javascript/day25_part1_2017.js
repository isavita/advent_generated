const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n\n');

const initialState = input[0].match(/Begin in state (\w)./)[1];
const diagnosticChecksum = parseInt(input[0].match(/Perform a diagnostic checksum after (\d+) steps./)[1], 10);

const states = {};
for (let i = 1; i < input.length; i++) {
  const lines = input[i].split('\n');
  const state = lines[0].match(/In state (\w):/)[1];
  states[state] = {
    0: {
      write: parseInt(lines[2].match(/Write the value (\d)./)[1], 10),
      move: lines[3].includes('right') ? 1 : -1,
      nextState: lines[4].match(/Continue with state (\w)./)[1],
    },
    1: {
      write: parseInt(lines[6].match(/Write the value (\d)./)[1], 10),
      move: lines[7].includes('right') ? 1 : -1,
      nextState: lines[8].match(/Continue with state (\w)./)[1],
    },
  };
}

let cursor = 0;
let state = initialState;
let tape = { 0: 0 };
let checksum = 0;

for (let i = 0; i < diagnosticChecksum; i++) {
  const value = tape[cursor] || 0;
  const { write, move, nextState } = states[state][value];

  tape[cursor] = write;
  cursor += move;
  state = nextState;
}

checksum = Object.values(tape).reduce((acc, val) => acc + val, 0);

console.log(checksum);