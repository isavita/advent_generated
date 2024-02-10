const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let horizontalPosition = 0;
let depth = 0;

data.forEach(line => {
    const [direction, units] = line.split(' ');
    const numUnits = parseInt(units);

    switch (direction) {
        case 'forward':
            horizontalPosition += numUnits;
            break;
        case 'down':
            depth += numUnits;
            break;
        case 'up':
            depth -= numUnits;
            break;
    }
});

const product = horizontalPosition * depth;
console.log(product);