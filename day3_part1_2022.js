const fs = require('fs');

function itemPriority(item) {
  if (item >= 'a' && item <= 'z') {
    return item.charCodeAt(0) - 'a'.charCodeAt(0) + 1;
  }
  return item.charCodeAt(0) - 'A'.charCodeAt(0) + 27;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }

  const lines = data.split('\n');
  let sum = 0;

  lines.forEach((line) => {
    const half = Math.floor(line.length / 2);
    const firstCompartment = line.slice(0, half);
    const secondCompartment = line.slice(half);

    const compartmentMap = {};
    for (const item of firstCompartment) {
      compartmentMap[item] = (compartmentMap[item] || 0) + 1;
    }

    for (const item of secondCompartment) {
      if (compartmentMap[item]) {
        sum += itemPriority(item);
        break;
      }
    }
  });

  console.log(sum);
});