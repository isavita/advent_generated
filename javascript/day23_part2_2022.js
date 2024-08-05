const fs = require('fs');

class P {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

class Elf {
    constructor(pos) {
        this.pos = pos;
        this.moving = false;
        this.nextPos = null;
    }

    aroundAllEmpty() {
        for (const d of Dirs) {
            const adj = new P(this.pos.x + d.x, this.pos.y + d.y);
            if (Map.has(adj.toString())) {
                return false;
            }
        }
        return true;
    }

    elfInDirection(wannaGo) {
        for (let j = -1; j <= 1; j++) {
            const dxy = Dirs[(wannaGo + j + 8) % 8];
            const adj = new P(this.pos.x + dxy.x, this.pos.y + dxy.y);
            if (Map.has(adj.toString())) {
                return true;
            }
        }
        return false;
    }
}

const N = 1, E = 3, S = 5, W = 7;
const Map = new Set();
const Elves = [];
const Order = [N, S, W, E];
let CurrDir = 0;
const Dirs = [
    new P(-1, -1), new P(-1, 0), new P(-1, 1), new P(0, 1),
    new P(1, 1), new P(1, 0), new P(1, -1), new P(0, -1)
];

function run() {
    const proposes = {};

    for (const e of Elves) {
        if (e.aroundAllEmpty()) {
            continue;
        }

        for (let i = 0; i < 4; i++) {
            const dir = Order[(CurrDir + i) % 4];

            if (e.elfInDirection(dir)) {
                continue;
            }

            const dxy = Dirs[dir];
            const dest = new P(e.pos.x + dxy.x, e.pos.y + dxy.y);
            proposes[dest.toString()] = (proposes[dest.toString()] || 0) + 1;
            e.nextPos = dest;
            e.moving = true;
            break;
        }
    }

    let someoneMoved = false;
    for (const e of Elves) {
        if (!e.moving) {
            continue;
        }

        if (proposes[e.nextPos.toString()] > 1) {
            e.moving = false;
            continue;
        }

        someoneMoved = true;
        Map.delete(e.pos.toString());
        Map.add(e.nextPos.toString());
        e.pos = e.nextPos;
        e.moving = false;
    }

    CurrDir = (CurrDir + 1) % 4;

    return someoneMoved;
}

function parse() {
    const data = fs.readFileSync('input.txt', 'utf8').split('\n');

    for (let row = 0; row < data.length; row++) {
        const line = data[row];
        for (let col = 0; col < line.length; col++) {
            if (line[col] === '#') {
                const p = new P(row, col);
                Map.add(p.toString());
                Elves.push(new Elf(p));
            }
        }
    }
}

P.prototype.toString = function() {
    return `${this.x},${this.y}`;
};

parse();

for (let i = 0; ; i++) {
    if (!run()) {
        console.log(i + 1);
        break;
    }
}