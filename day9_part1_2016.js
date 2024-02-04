const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

function decompressedLength(input) {
  let length = 0;
  let marker = '';
  let repeat = 0;
  
  for (let i = 0; i < input.length; i++) {
    if (input[i] === '(') {
      const endIndex = input.indexOf(')', i);
      marker = input.slice(i + 1, endIndex);
      const [characters, times] = marker.split('x').map(Number);
      repeat = times;
      i = endIndex + characters;
      length += characters * times;
    } else if (!marker) {
      length++;
    } else {
      repeat--;
      if (repeat === 0) {
        marker = '';
      }
    }
  }
  
  return length;
}

console.log(decompressedLength(input));