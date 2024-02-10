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
    console.error("Error opening file:", err);
    return;
  }

  let totalDiff = 0;
  data.split('\n').forEach(line => {
    const originalLength = line.length;
    const encodedLength = calculateEncodedLength(line);
    totalDiff += encodedLength - originalLength;
  });

  console.log(totalDiff);
});