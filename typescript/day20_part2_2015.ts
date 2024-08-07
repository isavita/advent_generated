import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf8').trim();
const target = Math.floor(parseInt(data) / 11);

const houses: number[] = new Array(target + 1).fill(0);

for (let elf = 1; elf <= target; elf++) {
    for (let house = elf; house <= Math.min(elf * 50, target); house += elf) {
        houses[house] += elf;
    }
}

for (let houseNumber = 0; houseNumber <= target; houseNumber++) {
    if (houses[houseNumber] >= target) {
        console.log(houseNumber);
        break;
    }
}