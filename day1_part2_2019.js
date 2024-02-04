const fs = require('fs');

function processLine(line) {
    const n = parseInt(line.trim());
    return isNaN(n) ? 0 : n;
}

function calcFuelMass(mass) {
    const fuel = Math.floor(mass / 3) - 2;
    return fuel <= 0 ? 0 : fuel + calcFuelMass(fuel);
}

function getTotal(masses) {
    let total = 0;
    for (let i = 0; i < masses.length; i++) {
        total += calcFuelMass(masses[i]);
    }
    return total;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading file');
        return;
    }

    const lines = data.trim().split('\n');
    const masses = lines.map(line => processLine(line));
    const total = getTotal(masses);

    console.log(total);
});