const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n').map(line => line.trim());
const replacements = [];
let molecule = '';

for (const line of data) {
  if (line === '') {
    continue;
  }
  if (line.includes(' => ')) {
    replacements.push(line);
  } else {
    molecule = line;
  }
}

const molecules = new Set();

for (const replacement of replacements) {
  const parts = replacement.split(' => ');
  for (let i = 0; i < molecule.length; i++) {
    if (molecule.startsWith(parts[0], i)) {
      const newMolecule = molecule.slice(0, i) + parts[1] + molecule.slice(i + parts[0].length);
      molecules.add(newMolecule);
    }
  }
}

console.log(molecules.size);