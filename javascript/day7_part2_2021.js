const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const positions = input.map(line => line.split(',').map(Number)).flat();

positions.sort((a, b) => a - b);

const calculateNewFuel = (currentPosition, newPosition) => {
    const diff = Math.abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
};

const abs = n => (n < 0 ? -n : n);

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