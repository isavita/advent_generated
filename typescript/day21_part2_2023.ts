import * as fs from 'fs';

const parseData = (data: string[]): [Map<string, boolean>, string] => {
    const garden = new Map<string, boolean>();
    let start = '';
    for (let y = 0; y < data.length; y++) {
        for (let x = 0; x < data[y].length; x++) {
            const c = data[y][x];
            if (c !== '#') {
                garden.set(`${x},${y}`, true);
            }
            if (c === 'S') {
                start = `${x},${y}`;
            }
        }
    }
    if (!start) throw new Error("No start found!");
    return [garden, start];
};

const complexMod = (num: string, mod: number): string => {
    const [x, y] = num.split(',').map(Number);
    return `${(x + 10 * mod) % mod},${(y + 10 * mod) % mod}`;
};

const calculateNumEnds = (garden: Map<string, boolean>, start: string, numIterations: number, maxSize: number): number => {
    let queue = new Map<string, boolean>();
    queue.set(start, true);
    const done: number[] = [];

    for (let i = 0; i < 3 * maxSize; i++) {
        if (i % maxSize === (maxSize - 1) / 2) {
            done.push(queue.size);
        }
        if (done.length === 3) break;

        const newQueue = new Map<string, boolean>();
        const directions = ['1,0', '-1,0', '0,1', '0,-1'];

        for (const dir of directions) {
            const [dx, dy] = dir.split(',').map(Number);
            for (const point of queue.keys()) {
                const [px, py] = point.split(',').map(Number);
                const newPoint = `${px + dx},${py + dy}`;
                if (garden.has(complexMod(newPoint, maxSize))) {
                    newQueue.set(newPoint, true);
                }
            }
        }
        queue = newQueue;
    }

    const quadraticFunction = (n: number, a: number, b: number, c: number): number => {
        return a + n * (b - a + ((n - 1) * (c - 2 * b + a) / 2));
    };

    return quadraticFunction(Math.floor(numIterations / maxSize), done[0], done[1], done[2]);
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const [garden, start] = parseData(data);
    const maxSize = data.length;
    const sum = calculateNumEnds(garden, start, 26501365, maxSize);
    console.log(sum);
};

main();