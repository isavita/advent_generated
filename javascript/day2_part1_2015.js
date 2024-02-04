const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let total = 0;

for (let i = 0; i < input.length; i++) {
    const dimensions = input[i].split('x');
    if (dimensions.length !== 3) {
        console.error('Invalid input format');
        process.exit(1);
    }

    const l = parseInt(dimensions[0]);
    const w = parseInt(dimensions[1]);
    const h = parseInt(dimensions[2]);

    const side1 = l * w;
    const side2 = w * h;
    const side3 = h * l;

    const smallest = Math.min(side1, side2, side3);
    total += 2 * side1 + 2 * side2 + 2 * side3 + smallest;
}

console.log(total);