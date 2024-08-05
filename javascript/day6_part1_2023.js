const fs = require('fs');

const calculateWaysToWin = (time, record) => {
  let waysToWin = 0;
  for (let holdTime = 1; holdTime < time; holdTime++) {
    const travelTime = time - holdTime;
    const distance = holdTime * travelTime;
    if (distance > record) {
      waysToWin++;
    }
  }
  return waysToWin;
};

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const times = input[0].split(' ').map(Number).filter(Boolean);
const distances = input[1].split(' ').map(Number).filter(Boolean);

let totalWays = 1;
for (let i = 0; i < times.length; i++) {
  const ways = calculateWaysToWin(times[i], distances[i]);
  totalWays *= ways;
}

console.log(totalWays);