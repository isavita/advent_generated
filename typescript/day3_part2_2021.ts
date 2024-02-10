
const fs = require('fs');

const filterValues = (values, criteria) => {
  for (let i = 0; i < values[0].length; i++) {
    let zeros = 0, ones = 0;
    for (const val of values) {
      if (val[i] === '0') {
        zeros++;
      } else {
        ones++;
      }
    }
    const keep = criteria(zeros, ones);
    values = filterByBit(values, i, keep);
    if (values.length === 1) {
      break;
    }
  }
  return values[0];
}

const filterByBit = (values, bitIndex, keep) => {
  const filtered = [];
  for (const val of values) {
    if (val[bitIndex] === keep) {
      filtered.push(val);
    }
  }
  return filtered;
}

const inputContent = fs.readFileSync('input.txt', 'utf-8');
const values = inputContent.trim().split('\n');

const oxygenGeneratorRating = filterValues(values, (zeros, ones) => zeros > ones ? '0' : '1');
const oxygenGeneratorRatingInt = parseInt(oxygenGeneratorRating, 2);

const co2ScrubberRating = filterValues(values, (zeros, ones) => zeros <= ones ? '0' : '1');
const co2ScrubberRatingInt = parseInt(co2ScrubberRating, 2);

console.log(oxygenGeneratorRatingInt * co2ScrubberRatingInt);
