const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

function decompressedLength(input, partTwo = false) {
  let length = 0;
  let marker = '';

  for (let i = 0; i < input.length; i++) {
    if (input[i] === '(') {
      let j = i + 1;
      while (input[j] !== ')') {
        marker += input[j];
        j++;
      }
      const [chars, repeat] = marker.split('x').map(Number);
      i = j + 1;

      const toRepeat = input.slice(i, i + chars);
      length += partTwo ? decompressedLength(toRepeat, true) * repeat : toRepeat.length * repeat;

      i += chars - 1;
      marker = '';
    } else {
      length++;
    }
  }

  return length;
}

console.log(decompressedLength(input)); // Part One
console.log(decompressedLength(input, true)); // Part Two