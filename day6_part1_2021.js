const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const fishStrs = input.split(',');

const fishes = new Array(9).fill(0);

fishStrs.forEach(fishStr => {
    const fish = parseInt(fishStr);
    fishes[fish]++;
});

for (let day = 1; day <= 80; day++) {
    const newFish = fishes[0];
    for (let i = 1; i < fishes.length; i++) {
        fishes[i - 1] = fishes[i];
    }
    fishes[6] += newFish;
    fishes[8] = newFish;
}

const totalFish = fishes.reduce((acc, fish) => acc + fish, 0);

console.log(totalFish);