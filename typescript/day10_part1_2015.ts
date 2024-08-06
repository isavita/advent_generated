const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const result = lookAndSay(input, 40);
console.log(result.length);

function lookAndSay(sequence, iterations) {
  for (let i = 0; i < iterations; i++) {
    sequence = nextSequence(sequence);
  }
  return sequence;
}

function nextSequence(sequence) {
  let result = '';
  for (let i = 0; i < sequence.length; ) {
    let count = 1;
    let digit = sequence[i];
    for (let j = i + 1; j < sequence.length && sequence[j] === digit; j++) {
      count++;
    }
    result += count + digit;
    i += count;
  }
  return result;
}