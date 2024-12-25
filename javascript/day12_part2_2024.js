
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const graph = input.split('\n').filter(line => line).map(line => line.split(''));
const H = graph.length;
const W = graph[0].length;

const move = [
    { label: 'left', x: -1, y: 0 },
    { label: 'up', x: 0, y: -1 },
    { label: 'right', x: 1, y: 0 },
    { label: 'down', x: 0, y: 1 },
];

let sum = 0;

for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
        if (graph[y][x] === '.') continue;

        let area = 0;
        const target = graph[y][x];
        const visited = new Set();
        const side = {};

        const search = (cx, cy, label) => {
            if (cy < 0 || cy >= H || cx < 0 || cx >= W || graph[cy][cx] !== target) {
                if (label && !visited.has(`${cx},${cy}`)) {
                    saveOuter(label, side, cx, cy);
                }
                return;
            }

            const key = `${cx},${cy}`;
            if (visited.has(key)) return;
            visited.add(key);
            area++;
            graph[cy][cx] = '.';

            for (const m of move) {
                const nx = cx + m.x;
                const ny = cy + m.y;
                search(nx, ny, m.label);
            }
        };

        search(x, y, '');
        const outer = countOuter(side);
        sum += area * outer;
    }
}

console.log(sum);

function saveOuter(label, side, x, y) {
    const key = (label === 'up' || label === 'down') ? `${y}:${x}` : `${x}:${y}`;
    side[label] = side[label] || new Set();
    side[label].add(key);
}

function countOuter(side) {
    let outer = 0;
    for (const label in side) {
        const array = Array.from(side[label]).sort((a, b) => {
            const [aFirst, aSecond] = a.split(':').map(Number);
            const [bFirst, bSecond] = b.split(':').map(Number);
            if (aFirst === bFirst) return aSecond - bSecond;
            return aFirst - bFirst;
        });

        const temp = new Set();
        for (const current of array) {
            const [i, j] = current.split(':').map(Number);
            if (!check(temp, i, j)) {
                outer++;
            }
            temp.add(current);
        }
    }
    return outer;
}

function check(ary, i, j) {
    return ary.has(`${i}:${j - 1}`) || ary.has(`${i}:${j + 1}`);
}
