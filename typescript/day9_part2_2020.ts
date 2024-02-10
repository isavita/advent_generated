const fs = require('fs');
const readline = require('readline');

const invalidNumber = 14360655;

async function main() {
  const fileStream = fs.createReadStream('input.txt');
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity
  });

  const numbers = [];

  for await (const line of rl) {
    numbers.push(parseInt(line));
  }

  for (let i = 0; i < numbers.length; i++) {
    let sum = numbers[i];
    let min = numbers[i];
    let max = numbers[i];
    for (let j = i + 1; j < numbers.length; j++) {
      sum += numbers[j];
      if (numbers[j] < min) {
        min = numbers[j];
      }
      if (numbers[j] > max) {
        max = numbers[j];
      }
      if (sum === invalidNumber) {
        console.log(min + max);
        return;
      } else if (sum > invalidNumber) {
        break;
      }
    }
  }
}

main();