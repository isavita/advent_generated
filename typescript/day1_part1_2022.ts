const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let maxCalories = 0;
let currentCalories = 0;

for (let i = 0; i < input.length; i++) {
  const line = input[i].trim();

  if (line === '') {
    if (currentCalories > maxCalories) {
      maxCalories = currentCalories;
    }
    currentCalories = 0;
    continue;
  }

  const calories = parseInt(line, 10);

  if (isNaN(calories)) {
    console.error(`Error converting line to int at line ${i + 1}`);
    process.exit(1);
  }

  currentCalories += calories;
}

if (currentCalories > maxCalories) {
  maxCalories = currentCalories;
}

console.log(maxCalories);