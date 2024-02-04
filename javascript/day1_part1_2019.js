const fs = require('fs');

let masses = [];
let total = 0;

function processLine(line) {
    const m = parseInt(line.trim());
    masses.push(m);
}

function getTotal() {
    let tempTotal = 0;

    for (let i = 0; i < masses.length; i++) {
        tempTotal += (Math.floor(masses[i] / 3) - 2);
    }

    total = tempTotal;
    return;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading file');
        return;
    }

    const lines = data.split('\n');
    lines.forEach(line => {
        processLine(line);
    });

    getTotal();

    console.log(total);
});