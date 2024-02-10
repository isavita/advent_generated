const fs = require('fs');

const masses = [];
let total = 0;

function processLine(line) {
    const m = parseInt(line.trim());
    if (isNaN(m)) {
        console.log("Error parsing line");
        return;
    }

    masses.push(m);
}

function getTotal() {
    let tempTotal = 0;

    masses.forEach(m => {
        tempTotal += (Math.floor(m / 3) - 2);
    });

    total = tempTotal;
    return;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error while reading file");
        return;
    }

    const lines = data.split('\n');
    lines.forEach(line => {
        processLine(line);
    });

    getTotal();

    console.log(total);
});