import * as fs from 'fs';

function calculateWaysToWin(time: number, record: number): number {
    let waysToWin = 0;
    for (let holdTime = 1; holdTime < time; holdTime++) {
        const travelTime = time - holdTime;
        const distance = holdTime * travelTime;
        if (distance > record) {
            waysToWin++;
        }
    }
    return waysToWin;
}

function main() {
    const data = fs.readFileSync('input.txt', 'utf-8').split('\n');
    const times = data[0].split(' ').map(Number).filter(Boolean);
    const distances = data[1].split(' ').map(Number).filter(Boolean);

    let totalWays = 1;
    for (let i = 0; i < times.length; i++) {
        const ways = calculateWaysToWin(times[i], distances[i]);
        totalWays *= ways;
    }

    console.log(totalWays);
}

main();