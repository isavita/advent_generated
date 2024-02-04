const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let maxCalories = 0;
let currentCalories = 0;

for (let i = 0; i < data.length; i++) {
    const line = data[i].trim();

    if (line === "") {
        if (currentCalories > maxCalories) {
            maxCalories = currentCalories;
        }
        currentCalories = 0;
        continue;
    }

    const calories = parseInt(line);

    if (isNaN(calories)) {
        console.error(`Error converting line to int: ${line}`);
        process.exit(1);
    }

    currentCalories += calories;
}

if (currentCalories > maxCalories) {
    maxCalories = currentCalories;
}

console.log(maxCalories);