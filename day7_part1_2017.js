const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');

const holderMap = new Map();
const heldMap = new Map();

const re = /[a-z]+/g;

for (const line of lines) {
  const names = line.match(re);
  const holder = names[0];
  holderMap.set(holder, true);

  if (names.length > 1) {
    for (let i = 1; i < names.length; i++) {
      heldMap.set(names[i], true);
    }
  }
}

for (const holder of holderMap.keys()) {
  if (!heldMap.has(holder)) {
    console.log(holder);
    break;
  }
}