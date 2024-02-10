
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
const positions = input.split(',').map(Number).sort((a, b) => a - b);

let minFuel = Number.MAX_SAFE_INTEGER;
for (let i = positions[0]; i <= positions[positions.length - 1]; i++) {
    let fuel = 0;
    for (const pos of positions) {
        fuel += calculateNewFuel(pos, i);
    }
    if (fuel < minFuel) {
        minFuel = fuel;
    }
}

console.log(minFuel);

function calculateNewFuel(currentPosition, newPosition) {
    const diff = Math.abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
}
