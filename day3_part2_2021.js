const fs = require('fs');

function filterValues(values, criteria) {
    for (let i = 0; i < values[0].length; i++) {
        let zeros = 0;
        let ones = 0;
        for (let val of values) {
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

function filterByBit(values, bitIndex, keep) {
    const filtered = [];
    for (let val of values) {
        if (val[bitIndex] === keep) {
            filtered.push(val);
        }
    }
    return filtered;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map(line => line.trim());

const oxygenGeneratorRating = filterValues(input, (zeros, ones) => {
    if (zeros > ones) {
        return '0';
    } else {
        return '1';
    }
});
const oxygenGeneratorRatingInt = parseInt(oxygenGeneratorRating, 2);

const co2ScrubberRating = filterValues(input, (zeros, ones) => {
    if (zeros <= ones) {
        return '0';
    } else {
        return '1';
    }
});
const co2ScrubberRatingInt = parseInt(co2ScrubberRating, 2);

console.log(oxygenGeneratorRatingInt * co2ScrubberRatingInt);