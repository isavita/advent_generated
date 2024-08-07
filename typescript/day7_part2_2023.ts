import * as fs from 'fs';

type Hand = {
  cards: string;
  bid: number;
};

const valueDict: { [key: string]: number } = {
  'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13
};

const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.split('\n');

const hands: Hand[] = [];

const cardRegex = /[\dAKQJT]+/;
const bidRegex = / [\d]+/;

for (const line of lines) {
  if (line.length === 0) continue;

  const cards = line.match(cardRegex)![0];
  const bid = parseInt(line.match(bidRegex)![0].trim());

  hands.push({ cards, bid });
}

const matches: Hand[][] = [[], [], [], [], [], [], []];

for (const hand of hands) {
  const count: { [key: string]: number } = {};

  for (const card of hand.cards) {
    count[card] = (count[card] || 0) + 1;
  }

  if (count['J'] > 0) {
    let highV = 0;
    let highKey = 'J';
    for (const key in count) {
      if (key !== 'J') {
        if (count[key] > highV) {
          highKey = key;
          highV = count[key];
        } else if (count[key] === highV && valueDict[key] > valueDict[highKey]) {
          highKey = key;
        }
      }
    }
    if (highKey !== 'J') {
      count[highKey] += count['J'];
      delete count['J'];
    }
  }

  let value = 1;
  for (const key in count) {
    value *= count[key];
  }

  switch (value) {
    case 1:
      matches[6].push(hand);
      break;
    case 2:
      matches[5].push(hand);
      break;
    case 3:
      matches[3].push(hand);
      break;
    case 4:
      matches[Object.keys(count).length === 2 ? 1 : 4].push(hand);
      break;
    case 5:
      matches[0].push(hand);
      break;
    case 6:
      matches[2].push(hand);
      break;
    default:
      console.log("oh no");
  }
}

const convertedMatches: number[][] = [];

for (const x of matches) {
  const temp: number[][] = [];
  for (const hand of x) {
    let y = hand.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, '1').replace(/Q/g, 'C').replace(/K/g, 'D');
    const val = parseInt(y, 16);
    temp.push([val, hand.bid]);
  }
  temp.sort((a, b) => b[0] - a[0]);
  convertedMatches.push(...temp);
}

let total = 0;
for (let x = 0; x < convertedMatches.length; x++) {
  total += convertedMatches[x][1] * (convertedMatches.length - x);
}

console.log(total);