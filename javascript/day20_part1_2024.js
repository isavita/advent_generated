
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

const isTrack = (x, y) => x >= 0 && x < h && y >= 0 && y < w && !walls[x][y];

let possibleCheats = 0;

for (const startPos of trackCells) {
    const sd = distFromS[startPos.x][startPos.y];
    if (sd === -1) continue;
    for (const d1 of dirs) {
        const m1 = { x: startPos.x + d1.x, y: startPos.y + d1.y };
        if (m1.x < 0 || m1.x >= h || m1.y < 0 || m1.y >= w) continue;
        for (const d2 of dirs) {
            const m2 = { x: m1.x + d2.x, y: m1.y + d2.y };
            if (m2.x < 0 || m2.x >= h || m2.y < 0 || m2.y >= w || !isTrack(m2.x, m2.y)) continue;
            const ed = distFromE[m2.x][m2.y];
            if (ed === -1) continue;
            const newCost = sd + 2 + ed;
            if (normalCost - newCost >= 100) possibleCheats++;
        }
    }
}

console.log(possibleCheats);
