const fs = require('fs');

function calculateEncodedLength(s) {
  let encoded = '"';
  for (let ch of s) {
    if (ch === '\\' || ch === '"') {
      encoded += '\\';
    }
    encoded += ch;
  }
  encoded += '"';
  return encoded.length;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }

  let totalDiff = 0;
  const lines = data.split('\n');
  for (let line of lines) {
    const originalLength = line.length;
    const encodedLength = calculateEncodedLength(line);
    totalDiff += encodedLength - originalLength;
  }

  console.log(totalDiff);
});