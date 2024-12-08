
const fs = require('node:fs');

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
    const rules = {};
    let updates = [];

    let i = 0;
    while (input[i].includes('|')) {
        const [a, b] = input[i].split('|').map(Number);
        rules[a] = rules[a] || [];
        rules[a].push(b);
        i++;
    }

    for (i++; i < input.length; i++) {
        updates.push(input[i].split(',').map(Number));
    }

    let sumOfMiddlePages = 0;
    for (const update of updates) {
        if (isCorrectOrder(update, rules)) {
            sumOfMiddlePages += update[Math.floor(update.length / 2)];
        }
    }

    console.log(sumOfMiddlePages);
}


function isCorrectOrder(update, rules) {
    for (let i = 0; i < update.length; i++) {
        for (let j = i + 1; j < update.length; j++) {
            const page1 = update[i];
            const page2 = update[j];
            if (rules[page1] && rules[page1].includes(page2)) continue;
            if (rules[page2] && rules[page2].includes(page1)) return false;

        }
    }
    return true;
}

solve();
