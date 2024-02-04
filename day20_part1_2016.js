const fs = require('fs');

class IPRange {
    constructor(start, end) {
        this.start = start;
        this.end = end;
    }
}

function readIPRanges(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const ranges = [];
    for (let i = 0; i < data.length; i++) {
        const parts = data[i].split('-');
        const start = parseInt(parts[0]);
        const end = parseInt(parts[1]);
        ranges.push(new IPRange(start, end));
    }
    return ranges;
}

function findUnblockedIP(ranges) {
    let currentIP = 0;
    for (let i = 0; i < ranges.length; i++) {
        const r = ranges[i];
        if (r.start > currentIP) {
            return currentIP;
        }
        if (r.end >= currentIP) {
            currentIP = r.end + 1;
        }
    }
    return currentIP;
}

const ipRanges = readIPRanges('input.txt');
ipRanges.sort((a, b) => a.start - b.start);

const unblockedIP = findUnblockedIP(ipRanges);
console.log(unblockedIP);