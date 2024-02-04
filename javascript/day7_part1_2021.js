const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const positions = input.map(line => line.split(',').map(Number)).flat();

positions.sort((a, b) => a - b);

const calculateFuel = (currentPosition, newPosition) => Math.abs(currentPosition - newPosition);

let minFuel = Number.MAX_SAFE_INTEGER;
for (let i = positions[0]; i <= positions[positions.length - 1]; i++) {
    let fuel = 0;
    for (let pos of positions) {
        fuel += calculateFuel(pos, i);
    }
    if (fuel < minFuel) {
        minFuel = fuel;
    }
}

console.log(minFuel);