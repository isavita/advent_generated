import * as fs from 'fs';

const SERIAL_NUMBER = parseInt(fs.readFileSync('input.txt', 'utf-8').trim());

const getPowerLevel = (x: number, y: number): number => {
    const rackID = x + 10;
    let powerLevel = rackID * y + SERIAL_NUMBER;
    powerLevel *= rackID;
    return Math.floor((powerLevel / 100) % 10) - 5;
};

const findMaxPowerSquare = (): { x: number; y: number; power: number } => {
    let maxPower = -Infinity;
    let coordinates = { x: 0, y: 0 };

    for (let x = 1; x <= 298; x++) {
        for (let y = 1; y <= 298; y++) {
            let totalPower = 0;
            for (let dx = 0; dx < 3; dx++) {
                for (let dy = 0; dy < 3; dy++) {
                    totalPower += getPowerLevel(x + dx, y + dy);
                }
            }
            if (totalPower > maxPower) {
                maxPower = totalPower;
                coordinates = { x, y };
            }
        }
    }

    return { ...coordinates, power: maxPower };
};

const result = findMaxPowerSquare();
console.log(`${result.x},${result.y}`);