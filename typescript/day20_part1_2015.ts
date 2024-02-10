
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const target = parseInt(data) / 10;

const houses = new Array(target + 1).fill(0);

for (let elf = 1; elf <= target; elf++) {
  for (let house = elf; house <= target; house += elf) {
    houses[house] += elf;
  }
}

for (let houseNumber = 0; houseNumber < houses.length; houseNumber++) {
  if (houses[houseNumber] >= target) {
    console.log(houseNumber);
    break;
  }
}
