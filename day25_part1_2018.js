const fs = require('fs');

class Point {
    constructor(x, y, z, t) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.t = t;
    }
}

function abs(x) {
    return x < 0 ? -x : x;
}

function manhattanDistance(a, b) {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t);
}

class UnionFind {
    constructor(size) {
        this.parent = Array.from({ length: size }, (_, i) => i);
    }

    find(x) {
        if (this.parent[x] !== x) {
            this.parent[x] = this.find(this.parent[x]);
        }
        return this.parent[x];
    }

    union(x, y) {
        const rootX = this.find(x);
        const rootY = this.find(y);
        if (rootX !== rootY) {
            this.parent[rootX] = rootY;
        }
    }
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const points = input.map(line => {
    const [x, y, z, t] = line.split(',').map(Number);
    return new Point(x, y, z, t);
});

const uf = new UnionFind(points.length);
for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < points.length; j++) {
        if (manhattanDistance(points[i], points[j]) <= 3) {
            uf.union(i, j);
        }
    }
}

let constellationCount = 0;
for (let i = 0; i < uf.parent.length; i++) {
    if (i === uf.parent[i]) {
        constellationCount++;
    }
}
console.log(constellationCount);