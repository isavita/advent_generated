
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let caloriesList = [];
let currentCalories = 0;

for (let i = 0; i < input.length; i++) {
    const line = input[i].trim();

    if (line === "") {
        caloriesList.push(currentCalories);
        currentCalories = 0;
        continue;
    }

    const calories = parseInt(line);

    if (isNaN(calories)) {
        throw new Error(`Error converting line to int: ${line}`);
    }

    currentCalories += calories;
}

caloriesList.push(currentCalories);
caloriesList.sort((a, b) => b - a);

let topThreeSum = 0;
for (let i = 0; i < 3 && i < caloriesList.length; i++) {
    topThreeSum += caloriesList[i];
}

console.log(topThreeSum);
