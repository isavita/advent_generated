import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const parseInput = (input: string[]) => {
  const histories: number[][] = [];
  for (const line of input) {
    const numbers = line.split(' ').map(Number);
    histories.push(numbers);
  }
  return histories;
};

const allZeros = (nums: number[]) => nums.every((num) => num === 0);

const calculateExtrapolation = (history: number[]) => {
  const extrapolations: number[] = [];
  for (let i = 1; i < history.length; i++) {
    const extrapolation = history[i] - history[i - 1];
    extrapolations.push(extrapolation);
  }
  return extrapolations;
};

const calculateExtrapolations = (history: number[]) => {
  const extrapolationsSeries: number[][] = [history];

  for (let i = 1; i < history.length; i++) {
    const previousExtrapolations = extrapolationsSeries[i - 1];
    if (allZeros(previousExtrapolations)) {
      return extrapolationsSeries;
    }

    const extrapolations = calculateExtrapolation(previousExtrapolations);
    extrapolationsSeries.push(extrapolations);
  }

  return extrapolationsSeries;
};

const solve = (input: string[]) => {
  const histories = parseInput(input);
  let res = 0;

  for (const history of histories) {
    const extrapolationsSeries = calculateExtrapolations(history);

    let futurePrediction = 0;
    for (let i = extrapolationsSeries.length - 1; i > -1; i--) {
      futurePrediction += extrapolationsSeries[i][extrapolationsSeries[i].length - 1];
    }

    res += futurePrediction;
  }

  return res;
};

console.log(solve(input));