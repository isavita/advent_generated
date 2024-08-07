import * as fs from 'fs';

const simulateLanternfish = (initialTimers: number[], days: number): number => {
    const fishCount = Array(9).fill(0);
    initialTimers.forEach(timer => fishCount[timer]++);

    for (let day = 0; day < days; day++) {
        const newFishes = fishCount[0];
        for (let i = 0; i < 8; i++) {
            fishCount[i] = fishCount[i + 1];
        }
        fishCount[6] += newFishes;
        fishCount[8] = newFishes;
    }

    return fishCount.reduce((sum, count) => sum + count, 0);
};

const readInput = (filename: string): number[] => {
    const data = fs.readFileSync(filename, 'utf-8');
    return data.trim().split(',').map(Number);
};

const main = () => {
    const initialTimers = readInput('input.txt');
    const totalFish = simulateLanternfish(initialTimers, 80);
    console.log(totalFish);
};

main();