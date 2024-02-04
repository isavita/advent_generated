const fs = require('fs');

const lanternFishCounts = new Array(9).fill(0);

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

input.forEach(line => {
  const fishAges = line.split(',');
  fishAges.forEach(age => {
    const ageCount = parseInt(age);
    lanternFishCounts[ageCount]++;
  });
});

for (let i = 0; i < 256; i++) {
  const newLanternFish = lanternFishCounts[0];
  for (let j = 0; j < lanternFishCounts.length - 1; j++) {
    lanternFishCounts[j] = lanternFishCounts[j + 1];
  }
  lanternFishCounts[6] += newLanternFish;
  lanternFishCounts[8] = newLanternFish;
}

const sum = lanternFishCounts.reduce((acc, num) => acc + num, 0);

console.log(sum);