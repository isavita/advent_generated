
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
const lanternFishCounts = Array(9).fill(0);

input.split('\n').forEach(line => {
  const fishAges = line.split(',');
  fishAges.forEach(age => {
    const ageCount = parseInt(age, 10);
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

console.log(lanternFishCounts.reduce((acc, curr) => acc + curr, 0));
