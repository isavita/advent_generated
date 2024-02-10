const fs = require('fs');

function abs(x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

function sign(x) {
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    }
    return 0;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8');
    const lines = input.trim().split('\n').map(line => line.split(' -> '));

    const overlaps = new Map();

    for (const line of lines) {
        const start = line[0].split(',');
        const end = line[1].split(',');

        const x1 = parseInt(start[0]);
        const y1 = parseInt(start[1]);
        const x2 = parseInt(end[0]);
        const y2 = parseInt(end[1]);

        const xStep = sign(x2 - x1);
        const yStep = sign(y2 - y1);
        let steps = abs(x2 - x1) + 1;
        if (abs(y2 - y1) > abs(x2 - x1)) {
            steps = abs(y2 - y1) + 1;
        }

        for (let i = 0; i < steps; i++) {
            const point = [x1 + i * xStep, y1 + i * yStep];
            overlaps.set(point.toString(), (overlaps.get(point.toString()) || 0) + 1);
        }
    }

    let count = 0;
    for (const v of overlaps.values()) {
        if (v > 1) {
            count++;
        }
    }

    console.log(count);
}

main();