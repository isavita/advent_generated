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

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading input file:', err);
        return;
    }

    const lines = data.trim().split('\n').map(line => {
        const parts = line.split(' -> ');
        const start = parts[0].split(',');
        const end = parts[1].split(',');

        const x1 = parseInt(start[0]);
        const y1 = parseInt(start[1]);
        const x2 = parseInt(end[0]);
        const y2 = parseInt(end[1]);

        return [x1, y1, x2, y2];
    });

    const overlaps = {};

    lines.forEach(line => {
        const [x1, y1, x2, y2] = line;

        const xStep = sign(x2 - x1);
        const yStep = sign(y2 - y1);
        let steps = abs(x2 - x1) + 1;
        if (abs(y2 - y1) > abs(x2 - x1)) {
            steps = abs(y2 - y1) + 1;
        }

        for (let i = 0; i < steps; i++) {
            const point = [x1 + i * xStep, y1 + i * yStep].toString();
            if (overlaps[point]) {
                overlaps[point]++;
            } else {
                overlaps[point] = 1;
            }
        }
    });

    let count = 0;
    Object.values(overlaps).forEach(v => {
        if (v > 1) {
            count++;
        }
    });

    console.log(count);
});