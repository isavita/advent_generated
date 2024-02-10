
const fs = require('fs');
const readline = require('readline');

async function main() {
  const fileStream = fs.createReadStream('input.txt');

  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity
  });

  let count = 0;

  for await (const line of rl) {
    const [pair1, pair2] = line.split(',');
    const [range1Start, range1End] = pair1.split('-').map(n => parseInt(n));
    const [range2Start, range2End] = pair2.split('-').map(n => parseInt(n));

    if (range1Start <= range2End && range1End >= range2Start) {
      count++;
    }
  }

  console.log(count);
}

main();
