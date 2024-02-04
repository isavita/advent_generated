const fs = require('fs');

function convertNumber(number, ranges) {
    for (let r of ranges) {
        if (number >= r.srcStart && number < r.srcStart + r.length) {
            return r.destStart + (number - r.srcStart);
        }
    }
    return number;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.split('\n');
    let seeds = [];
    let currentRanges = [];
    let maps = [];

    lines.forEach(line => {
        if (line.includes("map:")) {
            if (currentRanges.length > 0) {
                maps.push(currentRanges);
                currentRanges = [];
            }
        } else if (line.startsWith("seeds:")) {
            const seedStrs = line.substring(7).split(" ");
            seedStrs.forEach(s => {
                seeds.push(parseInt(s));
            });
        } else {
            const numbers = line.split(" ");
            if (numbers.length === 3) {
                const srcStart = parseInt(numbers[1]);
                const destStart = parseInt(numbers[0]);
                const length = parseInt(numbers[2]);

                currentRanges.push({ srcStart, destStart, length });
            }
        }
    });
    maps.push(currentRanges);

    let minLocation = -1;
    seeds.forEach(seed => {
        let location = seed;
        maps.forEach(m => {
            location = convertNumber(location, m);
        });

        if (minLocation === -1 || location < minLocation) {
            minLocation = location;
        }
    });

    console.log(minLocation);
});