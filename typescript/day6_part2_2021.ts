import * as fs from 'fs';

const simulateLanternfish = (initialState: number[], days: number): number => {
    const fishCount = new Array(9).fill(0);
    
    initialState.forEach(timer => fishCount[timer]++);
    
    for (let day = 0; day < days; day++) {
        const newFish = fishCount[0];
        for (let i = 0; i < 8; i++) {
            fishCount[i] = fishCount[i + 1];
        }
        fishCount[6] += newFish;
        fishCount[8] = newFish;
    }

    return fishCount.reduce((sum, count) => sum + count, 0);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const initialState = input.trim().split(',').map(Number);
    
    const totalFishAfter80Days = simulateLanternfish(initialState, 80);
    const totalFishAfter256Days = simulateLanternfish(initialState, 256);
    
    console.log(`Total lanternfish after 80 days: ${totalFishAfter80Days}`);
    console.log(`Total lanternfish after 256 days: ${totalFishAfter256Days}`);
};

main();