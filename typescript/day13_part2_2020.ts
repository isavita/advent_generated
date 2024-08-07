import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const earliestTimestamp = parseInt(input[0]);
const busIds = input[1].split(',').map(id => (id === 'x' ? null : parseInt(id)));

// Part 1: Find the earliest bus
const [earliestBusId, waitTime] = busIds
  .filter(id => id !== null)
  .map(id => [id, id - (earliestTimestamp % id)] as [number, number])
  .reduce((min, curr) => curr[1] < min[1] ? curr : min);

const part1Result = earliestBusId * waitTime;
console.log(`Part 1: ${part1Result}`);

// Part 2: Find the earliest timestamp for the bus schedule
const busSchedule = busIds.map((id, index) => (id ? { id, offset: index } : null)).filter(Boolean) as { id: number; offset: number }[];

let timestamp = 0;
let step = busSchedule[0].id;

for (let i = 1; i < busSchedule.length; i++) {
  const { id, offset } = busSchedule[i];
  while ((timestamp + offset) % id !== 0) {
    timestamp += step;
  }
  step *= id;
}

console.log(`Part 2: ${timestamp}`);