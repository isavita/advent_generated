
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const h = input.length;
const w = input[0].length;
let S, E;
const walls = Array(h).fill(null).map(() => Array(w).fill(false));
const trackCells = [];

for (let i = 0; i < h; i++) {
    for (let j = 0; j < w; j++) {
        const ch = input[i][j];
        if (ch === 'S') S = { x: i, y: j };
        else if (ch === 'E') E = { x: i, y: j };
        if (ch === '#') walls[i][j] = true;
        else trackCells.push({ x: i, y: j });
    }
}

const dirs = [{ x: 1, y: 0 }, { x: -1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 }];
const isTrack = (x, y) => x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];

const normalDistFrom = (start) => {
    const dist = Array(h).fill(null).map(() => Array(w).fill(-1));
    dist[start.x][start.y] = 0;
    const q = [start];
    while (q.length > 0) {
        const cur = q.shift();
        for (const d of dirs) {
            const nx = cur.x + d.x;
            const ny = cur.y + d.y;
            if (nx < 0 || nx >= h || ny < 0 || ny >= w || walls[nx][ny] || dist[nx][ny] !== -1) continue;
            dist[nx][ny] = dist[cur.x][cur.y] + 1;
            q.push({ x: nx, y: ny });
        }
    }
    return dist;
};

const distFromS = normalDistFrom(S);
const distFromE = normalDistFrom(E);
if (distFromS[E.x][E.y] === -1) {
    console.log(0);
    return;
}
const normalCost = distFromS[E.x][E.y];

const cheats = new Map();

for (const startPos of trackCells) {
    const sd = distFromS[startPos.x][startPos.y];
    if (sd === -1) continue;

    const distC = Array(h).fill(null).map(() => Array(w).fill(-1));
    distC[startPos.x][startPos.y] = 0;
    const q = [startPos];

    while (q.length > 0) {
        const cur = q.shift();
        const steps = distC[cur.x][cur.y];
        if (steps === 20) continue;
        for (const d of dirs) {
            const nx = cur.x + d.x;
            const ny = cur.y + d.y;
            if (nx < 0 || nx >= h || ny < 0 || ny >= w || distC[nx][ny] !== -1) continue;
            distC[nx][ny] = steps + 1;
            q.push({ x: nx, y: ny });
        }
    }

    for (let x = 0; x < h; x++) {
        for (let y = 0; y < w; y++) {
            const s = distC[x][y];
            if (s > 0 && s <= 20 && isTrack(x, y)) {
                const ed = distFromE[x][y];
                if (ed === -1) continue;
                const cost = sd + s + ed;
                if (cost < normalCost) {
                    const key = `${startPos.x},${startPos.y},${x},${y}`;
                    const old = cheats.get(key);
                    if (old === undefined || cost < old) {
                        cheats.set(key, cost);
                    }
                }
            }
        }
    }
}

let count = 0;
for (const cost of cheats.values()) {
    if (normalCost - cost >= 100) {
        count++;
    }
}
console.log(count);
