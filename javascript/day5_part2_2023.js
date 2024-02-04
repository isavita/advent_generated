const fs = require('fs');

function reverseConvertNumber(number, ranges) {
    for (let i = ranges.length - 1; i >= 0; i--) {
        const r = ranges[i];
        if (number >= r.destStart && number < r.destStart + r.length) {
            return r.srcStart + (number - r.destStart);
        }
    }
    return number;
}

function isInSeedRanges(number, ranges) {
    for (const r of ranges) {
        if (number >= r[0] && number < r[0] + r[1]) {
            return true;
        }
    }
    return false;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
let seedRanges = [];
let currentRanges = [];
let maps = [];

for (const line of input) {
    if (line.includes("map:")) {
        if (currentRanges.length > 0) {
            maps.push(currentRanges);
            currentRanges = [];
        }
    } else if (line.startsWith("seeds:")) {
        const seedStrs = line.slice(7).split(" ");
        for (let i = 0; i < seedStrs.length; i += 2) {
            const start = parseInt(seedStrs[i]);
            const length = parseInt(seedStrs[i + 1]);
            seedRanges.push([start, length]);
        }
    } else {
        const numbers = line.split(" ");
        if (numbers.length === 3) {
            const srcStart = parseInt(numbers[1]);
            const destStart = parseInt(numbers[0]);
            const length = parseInt(numbers[2]);
            currentRanges.push({ srcStart, destStart, length });
        }
    }
}
if (currentRanges.length > 0) {
    maps.push(currentRanges);
}

for (let location = 0; ; location++) {
    let seed = location;
    for (let i = maps.length - 1; i >= 0; i--) {
        seed = reverseConvertNumber(seed, maps[i]);
    }

    if (isInSeedRanges(seed, seedRanges)) {
        console.log(location);
        break;
    }
}