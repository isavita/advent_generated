const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const frequencyChanges = data.trim().split('\n');
const frequencies = new Set();
let currentFrequency = 0;
frequencies.add(currentFrequency);

while (true) {
  for (const change of frequencyChanges) {
    const frequencyDelta = parseInt(change);
    currentFrequency += frequencyDelta;
    if (frequencies.has(currentFrequency)) {
      console.log(currentFrequency);
      process.exit();
    }
    frequencies.add(currentFrequency);
  }
}