const fs = require('fs');

function calculateMemoryLength(s) {
  let length = 0;
  let inEscape = false;
  let hexCount = 0;

  for (let i = 1; i < s.length - 1; i++) {
    switch (true) {
      case hexCount > 0:
        hexCount--;
        break;
      case inEscape:
        if (s[i] === 'x') {
          hexCount = 2;
        }
        inEscape = false;
        length++;
        break;
      case s[i] === '\\':
        inEscape = true;
        break;
      default:
        length++;
    }
  }
  return length;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }

  const lines = data.split('\n');
  let totalDiff = 0;

  lines.forEach((line) => {
    const codeLength = line.length;
    const memoryLength = calculateMemoryLength(line);
    totalDiff += codeLength - memoryLength;
  });

  console.log(totalDiff);
});