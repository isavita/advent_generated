
import { readFileSync } from 'fs';

type Box = { x: bigint; y: bigint; z: bigint };
type Edge = { u: number; v: number; d: bigint };

class DSU {
    p: number[];
    c: number;
    constructor(n: number) {
        this.p = new Array(n);
        for (let i = 0; i < n; i++) this.p[i] = i;
        this.c = n;
    }
    find(i: number): number {
        let root = i;
        while (this.p[root] !== root) root = this.p[root];
        while (this.p[i] !== root) {
            const nxt = this.p[i];
            this.p[i] = root;
            i = nxt;
        }
        return root;
    }
    union(a: number, b: number): boolean {
        const ra = this.find(a), rb = this.find(b);
        if (ra === rb) return false;
        this.p[ra] = rb;
        this.c--;
        return true;
    }
}

function main() {
    const data = readFileSync('input.txt', 'utf8').trim().split(/\r?\n/);
    const boxes: Box[] = [];
    for (const line of data) {
        if (!line) continue;
        const parts = line.split(',').map(s => s.trim());
        if (parts.length < 3) continue;
        boxes.push({
            x: BigInt(parts[0]),
            y: BigInt(parts[1]),
            z: BigInt(parts[2])
        });
    }
    const n = boxes.length;
    if (n < 2) return;

    const edges: Edge[] = [];
    for (let i = 0; i < n; i++) {
        const b1 = boxes[i];
        for (let j = i + 1; j < n; j++) {
            const b2 = boxes[j];
            const dx = b1.x - b2.x;
            const dy = b1.y - b2.y;
            const dz = b1.z - b2.z;
            const d = dx * dx + dy * dy + dz * dz;
            edges.push({ u: i, v: j, d });
        }
    }

    edges.sort((a, b) => (a.d < b.d ? -1 : a.d > b.d ? 1 : 0));

    const dsu = new DSU(n);
    for (const e of edges) {
        if (dsu.union(e.u, e.v) && dsu.c === 1) {
            const res = boxes[e.u].x * boxes[e.v].x;
            console.log(res.toString());
            return;
        }
    }
}

main();
