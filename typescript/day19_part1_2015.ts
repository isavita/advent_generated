const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const replacements = [];
let molecule;

for (const line of input) {
  if (!line) continue;
  if (line.includes(' => ')) {
    replacements.push(line);
  } else {
    molecule = line;
  }
}

const molecules = new Set();

for (const replacement of replacements) {
  const [from, to] = replacement.split(' => ');
  for (let i = 0; i < molecule.length; i++) {
    if (molecule.startsWith(from, i)) {
      const newMolecule = molecule.slice(0, i) + to + molecule.slice(i + from.length);
      molecules.add(newMolecule);
    }
  }
}

console.log(molecules.size);