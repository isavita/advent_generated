const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n').filter(Boolean);

const firewall = {};
for (const line of lines) {
    const [depth, range] = line.split(': ').map(Number);
    firewall[depth] = { Range: range, Position: 0, Direction: 1 };
}

let delay = 0;
while (true) {
    if (passThrough(firewall, delay)) {
        break;
    }
    delay++;
}

console.log(delay);

function passThrough(firewall, delay) {
    for (const [depth, scanner] of Object.entries(firewall)) {
        if ((parseInt(depth) + delay) % (2 * (scanner.Range - 1)) === 0) {
            return false;
        }
    }
    return true;
}