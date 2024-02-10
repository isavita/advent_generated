const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map(Number);

let caloriesList = [];
let currentCalories = 0;

for (let i = 0; i < input.length; i++) {
  if (input[i] === 0 || isNaN(input[i])) {
    caloriesList.push(currentCalories);
    currentCalories = 0;
    continue;
  }

  currentCalories += input[i];
}

caloriesList.push(currentCalories);
caloriesList.sort((a, b) => b - a);

let topThreeSum = 0;
for (let i = 0; i < 3 && i < caloriesList.length; i++) {
  topThreeSum += caloriesList[i];
}

console.log(topThreeSum);