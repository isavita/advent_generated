const fs = require('fs');

function parseData(data) {
    const garden = new Map();
    let start = null;
    data.forEach((line, y) => {
        for (let x = 0; x < line.length; x++) {
            const c = line[x];
            if (c !== '#') {
                garden.set(`${x},${y}`, true);
            }
            if (c === 'S') {
                start = `${x},${y}`;
            }
        }
    });
    if (start === null) {
        throw new Error("No start found!");
    }
    return [garden, start];
}

function complexMod(num, mod) {
    const parts = num.split(',').map(Number);
    if (!Number.isInteger(parts[0]) || !Number.isInteger(parts[1])) {
        console.error("Complex number not integer!");
        throw new Error("Complex number not integer!");
    }
    return `${(parts[0] + 10 * mod) % mod},${(parts[1] + 10 * mod) % mod}`;
}

function calculateNumEnds(garden, start, numIterations, maxSize) {
    let queue = new Map();
    queue.set(start, true);

    let done = [];

    for (let i = 0; i < 3 * maxSize; i++) {
        if (i % maxSize === (maxSize - 1) / 2) {
            done.push(queue.size);
        }
        if (done.length === 3) {
            break;
        }

        let newQueue = new Map();

        [[1,0], [-1,0], [0,1], [0,-1]].forEach(dir => {
            Array.from(queue.keys()).forEach(point => {
                const parts = point.split(',').map(Number);
                const newPoint = `${parts[0] + dir[0]},${parts[1] + dir[1]}`;
                if (garden.has(complexMod(newPoint, maxSize))) {
                    newQueue.set(newPoint, true);
                }
            });
        });

        queue = newQueue;
    }

    const quadraticFunction = (n, a, b, c) => {
        return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2));
    };

    return quadraticFunction(Math.floor(numIterations / maxSize), done[0], done[1], done[2]);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error opening file:", err);
        return;
    }

    const lines = data.split('\n').filter(line => line.length);
    const [garden, start] = parseData(lines);
    const maxSize = lines.length;

    const result = calculateNumEnds(garden, start, 26501365, maxSize);
    console.log(result);
});