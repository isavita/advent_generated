const fs = require('fs');

class Bot {
  constructor(lowTo, highTo) {
    this.lowTo = lowTo;
    this.highTo = highTo;
    this.chips = [];
  }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const bots = {};

const valueRegex = /value (\d+) goes to (bot \d+)/;
const givesRegex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/;

for (let line of input) {
  if (valueRegex.test(line)) {
    const [, value, botID] = line.match(valueRegex);
    if (!bots[botID]) bots[botID] = new Bot();
    bots[botID].chips.push(parseInt(value));
  } else if (givesRegex.test(line)) {
    const [, botID, lowTo, highTo] = line.match(givesRegex);
    if (!bots[botID]) bots[botID] = new Bot();
    bots[botID].lowTo = lowTo;
    bots[botID].highTo = highTo;
  }
}

while (true) {
  let action = false;
  for (let botID in bots) {
    let b = bots[botID];
    if (b.chips.length === 2) {
      action = true;
      const [low, high] = [Math.min(...b.chips), Math.max(...b.chips)];
      if (low === 17 && high === 61) {
        console.log(botID);
        process.exit();
      }
      b.chips = [];
      giveChip(bots, b.lowTo, low);
      giveChip(bots, b.highTo, high);
    }
  }
  if (!action) break;
}

function giveChip(bots, target, value) {
  if (!bots[target]) bots[target] = new Bot();
  bots[target].chips.push(value);
}