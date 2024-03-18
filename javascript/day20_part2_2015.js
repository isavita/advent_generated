const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const target = parseInt(input) / 11;

const houses = Array.from({ length: target + 1 }, () => 0);

for (let elf = 1; elf <= target; elf++) {
  for (let house = elf; house <= elf * 50 && house <= target; house += elf) {
    houses[house] += elf;
  }
}

for (let houseNumber = 0; houseNumber < houses.length; houseNumber++) {
  if (houses[houseNumber] >= target) {
    console.log(houseNumber);
    break;
  }
}