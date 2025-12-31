
import { readFileSync } from 'fs';

type Point = { x: number; y: number; z: number };
type Edge = { u: number; v: number; d: number };

function find(parent: number[], x: number): number {
    while (parent[x] !== x) {
        parent[x] = parent[parent[x]];
        x = parent[x];
    }
    return x;
}

function union(parent: number[], size: number[], a: number, b: number): void {
    let ra = find(parent, a);
    let rb = find(parent, b);
    if (ra === rb) return;
    if (size[ra] < size[rb]) [ra, rb] = [rb, ra];
    parent[rb] = ra;
    size[ra] += size[rb];
}

function main() {
    const content = readFileSync('input.txt', 'utf8');
    const pts: Point[] = [];
    for (const line of content.split('\n')) {
        const m = line.match(/^\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*$/);
        if (!m) continue;
        pts.push({ x: +m[1], y: +m[2], z: +m[3] });
    }
    const n = pts.length;
    if (n < 2) {
        console.log('Not enough points to form circuits.');
        return;
    }

    const edges: Edge[] = [];
    for (let i = 0; i < n; ++i) {
        for (let j = i + 1; j < n; ++j) {
            const dx = pts[i].x - pts[j].x;
            const dy = pts[i].y - pts[j].y;
            const dz = pts[i].z - pts[j].z;
            edges.push({ u: i, v: j, d: dx * dx + dy * dy + dz * dz });
        }
    }
    edges.sort((a, b) => a.d - b.d);

    const parent = new Array<number>(n);
    const sz = new Array<number>(n);
    for (let i = 0; i < n; ++i) {
        parent[i] = i;
        sz[i] = 1;
    }

    const limit = Math.min(edges.length, 1000);
    for (let i = 0; i < limit; ++i) {
        const e = edges[i];
        union(parent, sz, e.u, e.v);
    }

    const top: number[] = [0, 0, 0];
    for (let i = 0; i < n; ++i) {
        if (parent[i] === i) {
            const s = sz[i];
            if (s > top[0]) {
                top[2] = top[1];
                top[1] = top[0];
                top[0] = s;
            } else if (s > top[1]) {
                top[2] = top[1];
                top[1] = s;
            } else if (s > top[2]) {
                top[2] = s;
            }
        }
    }

    let result = 1n;
    for (let i = 0; i < 3 && top[i] > 0; ++i) result *= BigInt(top[i]);

    console.log(`Product of three largest circuit sizes: ${result}`);
}

main();
