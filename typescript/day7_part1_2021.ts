const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const positions = input.map(line => line.split(',').map(Number)).flat().sort((a, b) => a - b);

let min_fuel = Number.MAX_SAFE_INTEGER;
for (let i = positions[0]; i <= positions[positions.length - 1]; i++) {
    let fuel = 0;
    for (let pos of positions) {
        fuel += calculateFuel(pos, i);
    }
    if (fuel < min_fuel) {
        min_fuel = fuel;
    }
}

console.log(min_fuel);

function calculateFuel(currentPosition, newPosition) {
    return Math.abs(currentPosition - newPosition);
}