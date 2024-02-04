const fs = require('fs');

class Scanner {
    constructor(range, position, direction) {
        this.range = range;
        this.position = position;
        this.direction = direction;
    }
}

const file = fs.readFileSync('input.txt', 'utf8').split('\n');
const firewall = {};

file.forEach(line => {
    const [depth, range] = line.split(': ');
    firewall[parseInt(depth)] = new Scanner(parseInt(range), 0, 1);
});

let severity = 0;

for (let depth = 0; depth <= maxDepth(firewall); depth++) {
    if (firewall[depth]) {
        if (firewall[depth].position === 0) {
            severity += depth * firewall[depth].range;
        }
    }

    Object.values(firewall).forEach(scanner => {
        moveScanner(scanner);
    });
}

console.log(severity);

function maxDepth(firewall) {
    let max = 0;
    Object.keys(firewall).forEach(depth => {
        if (parseInt(depth) > max) {
            max = parseInt(depth);
        }
    });
    return max;
}

function moveScanner(scanner) {
    if (scanner.position === 0) {
        scanner.direction = 1;
    } else if (scanner.position === scanner.range - 1) {
        scanner.direction = -1;
    }
    scanner.position += scanner.direction;
}