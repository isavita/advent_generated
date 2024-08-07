const fs = require('fs');

function parseInput(input: string[]) {
  return input.map(line => line.split(' ').map(Number));
}

function allZeros(nums: number[]) {
  return nums.every(num => num === 0);
}

function calculateExtrapolation(history: number[]) {
  return history.slice(1).map((num, i) => num - history[i]);
}

function calculateExtrapolations(history: number[]) {
  const extrapolationsSeries = [history];
  while (true) {
    const previousExtrapolations = extrapolationsSeries[extrapolationsSeries.length - 1];
    if (allZeros(previousExtrapolations)) break;
    const extrapolations = calculateExtrapolation(previousExtrapolations);
    extrapolationsSeries.push(extrapolations);
  }
  return extrapolationsSeries;
}

function solve(input: string[]) {
  const histories = parseInput(input);
  let res = 0;
  for (const history of histories) {
    const extrapolationsSeries = calculateExtrapolations(history);
    let pastPrediction = 0;
    for (let i = extrapolationsSeries.length - 1; i >= 0; i--) {
      pastPrediction = extrapolationsSeries[i][0] - pastPrediction;
    }
    res += pastPrediction;
  }
  return res;
}

function readFile(fileName: string) {
  return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));