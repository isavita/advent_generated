import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const earliestTimestamp = Number(input[0]);
const busIDs = input[1].split(',').filter(id => id !== 'x').map(Number);

let earliestBusID = 0;
let minWaitTime = Infinity;

for (const busID of busIDs) {
    const waitTime = busID - (earliestTimestamp % busID);
    if (waitTime < minWaitTime) {
        minWaitTime = waitTime;
        earliestBusID = busID;
    }
}

console.log(earliestBusID * minWaitTime);