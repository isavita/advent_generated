const fs = require('fs');

class Disc {
    constructor(totalPositions, startPosition) {
        this.totalPositions = totalPositions;
        this.startPosition = startPosition;
    }
}

const discs = [];
const discRegex = /Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./;

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
input.forEach(line => {
    const matches = discRegex.exec(line);
    const totalPositions = parseInt(matches[2]);
    const startPosition = parseInt(matches[3]);
    discs.push(new Disc(totalPositions, startPosition));
});

discs.push(new Disc(11, 0));

let time = 0;
while (true) {
    if (checkDiscs(discs, time)) {
        console.log(time);
        break;
    }
    time++;
}

function checkDiscs(discs, time) {
    for (let i = 0; i < discs.length; i++) {
        const disc = discs[i];
        const position = (disc.startPosition + time + i + 1) % disc.totalPositions;
        if (position !== 0) {
            return false;
        }
    }
    return true;
}