const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const earliestDeparture = Number(input[0]);
const busIDs = input[1].split(',');

let earliestBusID = 0;
let minWaitTime = earliestDeparture;

for (const id of busIDs) {
  if (id === 'x') {
    continue;
  }
  const busID = Number(id);
  const waitTime = busID - (earliestDeparture % busID);
  if (waitTime < minWaitTime) {
    minWaitTime = waitTime;
    earliestBusID = busID;
  }
}

console.log(earliestBusID * minWaitTime);