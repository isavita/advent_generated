const fs = require('fs');

class Scanner {
    constructor(range, position, direction) {
        this.Range = range;
        this.Position = position;
        this.Direction = direction;
    }
}

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n');

const firewall = new Map();

lines.forEach(line => {
    const [depth, range] = line.split(': ');
    firewall.set(parseInt(depth), new Scanner(parseInt(range), 0, 1));
});

let delay = 0;
while (true) {
    if (passThrough(firewall, delay)) {
        break;
    }
    delay++;
}

console.log(delay);

function passThrough(firewall, delay) {
    for (const [depth, scanner] of firewall) {
        if ((depth + delay) % (2 * (scanner.Range - 1)) === 0) {
            return false;
        }
    }
    return true;
}