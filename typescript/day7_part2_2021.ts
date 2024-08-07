import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const positions = input.split(',').map(Number);

// Part One: Constant fuel cost
const calculateFuelPartOne = (target: number): number => {
    return positions.reduce((total, pos) => total + Math.abs(pos - target), 0);
};

const findOptimalPositionPartOne = (): number => {
    const min = Math.min(...positions);
    const max = Math.max(...positions);
    let minFuel = Infinity;

    for (let target = min; target <= max; target++) {
        const fuel = calculateFuelPartOne(target);
        if (fuel < minFuel) {
            minFuel = fuel;
        }
    }
    return minFuel;
};

// Part Two: Increasing fuel cost
const calculateFuelPartTwo = (target: number): number => {
    return positions.reduce((total, pos) => {
        const distance = Math.abs(pos - target);
        return total + (distance * (distance + 1)) / 2; // Sum of first n numbers: n(n + 1) / 2
    }, 0);
};

const findOptimalPositionPartTwo = (): number => {
    const min = Math.min(...positions);
    const max = Math.max(...positions);
    let minFuel = Infinity;

    for (let target = min; target <= max; target++) {
        const fuel = calculateFuelPartTwo(target);
        if (fuel < minFuel) {
            minFuel = fuel;
        }
    }
    return minFuel;
};

const resultPartOne = findOptimalPositionPartOne();
const resultPartTwo = findOptimalPositionPartTwo();

console.log(`Minimum fuel for part one: ${resultPartOne}`);
console.log(`Minimum fuel for part two: ${resultPartTwo}`);