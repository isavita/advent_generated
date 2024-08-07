import * as fs from 'fs';

const calculateFuelCost = (positions: number[], target: number): number => {
    return positions.reduce((total, pos) => total + Math.abs(pos - target), 0);
};

const findOptimalPosition = (positions: number[]): number => {
    const minPos = Math.min(...positions);
    const maxPos = Math.max(...positions);
    let minFuel = Infinity;
    let optimalPosition = minPos;

    for (let target = minPos; target <= maxPos; target++) {
        const fuelCost = calculateFuelCost(positions, target);
        if (fuelCost < minFuel) {
            minFuel = fuelCost;
            optimalPosition = target;
        }
    }

    return minFuel;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const positions = input.split(',').map(Number);
    const result = findOptimalPosition(positions);
    console.log(result);
};

main();