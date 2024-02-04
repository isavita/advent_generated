const fs = require('fs');

const expenses = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

for (let i = 0; i < expenses.length; i++) {
    for (let j = i + 1; j < expenses.length; j++) {
        for (let k = j + 1; k < expenses.length; k++) {
            if (expenses[i] + expenses[j] + expenses[k] === 2020) {
                console.log(expenses[i] * expenses[j] * expenses[k]);
                process.exit();
            }
        }
    }
}