import * as fs from 'fs';

type Coord = { x: number; y: number };

class GeologicMap {
    private geologicIndicesCache: Map<number, Map<number, number>> = new Map();
    private erosionLevelsCache: Map<number, Map<number, number>> = new Map();
    constructor(public depth: number, public target: Coord) {}

    private geologicIndex(x: number, y: number): number {
        if (this.geologicIndicesCache.has(y) && this.geologicIndicesCache.get(y)!.has(x)) {
            return this.geologicIndicesCache.get(y)!.get(x)!;
        }
        let index: number;
        if ((x === 0 && y === 0) || (x === this.target.x && y === this.target.y)) {
            index = 0;
        } else if (y === 0) {
            index = x * 16807;
        } else if (x === 0) {
            index = y * 48271;
        } else {
            index = this.erosionLevel(x - 1, y) * this.erosionLevel(x, y - 1);
        }
        if (!this.geologicIndicesCache.has(y)) this.geologicIndicesCache.set(y, new Map());
        this.geologicIndicesCache.get(y)!.set(x, index);
        return index;
    }

    private erosionLevel(x: number, y: number): number {
        if (this.erosionLevelsCache.has(y) && this.erosionLevelsCache.get(y)!.has(x)) {
            return this.erosionLevelsCache.get(y)!.get(x)!;
        }
        const level = (this.geologicIndex(x, y) + this.depth) % 20183;
        if (!this.erosionLevelsCache.has(y)) this.erosionLevelsCache.set(y, new Map());
        this.erosionLevelsCache.get(y)!.set(x, level);
        return level;
    }

    public type(x: number, y: number): number {
        return this.erosionLevel(x, y) % 3;
    }

    public neighbors(pos: Coord, equip: number): Array<Item> {
        const n: Item[] = [];
        const directions = [{ x: 1, y: 0 }, { x: 0, y: 1 }, { x: -1, y: 0 }, { x: 0, y: -1 }];
        for (const dir of directions) {
            const c = { x: pos.x + dir.x, y: pos.y + dir.y };
            if (c.x < 0 || c.y < 0) continue;
            const t = this.type(c.x, c.y);
            if (equip & this.allowed(t)) {
                n.push({ pos: c, equip, time: 1 });
                n.push({ pos: c, equip: equip ^ this.allowed(t), time: 8 });
            }
        }
        return n;
    }

    private allowed(regionType: number): number {
        switch (regionType) {
            case 0: return 6; // Gear | Torch
            case 1: return 5; // Gear | None
            case 2: return 3; // Torch | None
            default: throw new Error(`unknown region type: ${regionType}`);
        }
    }
}

type Item = { pos: Coord; equip: number; time: number };

class PriorityQueue {
    private elements: Item[] = [];
    public push(item: Item) {
        this.elements.push(item);
        this.elements.sort((a, b) => a.time - b.time);
    }
    public pop(): Item {
        return this.elements.shift()!;
    }
    public isEmpty(): boolean {
        return this.elements.length === 0;
    }
}

const bailFactor = 8;

function rescue(input: string): number {
    const [depthLine, targetLine] = input.split('\n');
    const depth = parseInt(depthLine.split(': ')[1]);
    const [targetX, targetY] = targetLine.split(': ')[1].split(',').map(Number);
    const m = new GeologicMap(depth, { x: targetX, y: targetY });

    const queue = new PriorityQueue();
    queue.push({ pos: { x: 0, y: 0 }, time: 0, equip: 2 });
    const distances = new Map<string, number>();
    distances.set(JSON.stringify({ x: 0, y: 0, equip: 2 }), 0);

    while (!queue.isEmpty()) {
        const item = queue.pop();
        if (item.pos.x === m.target.x && item.pos.y === m.target.y && item.equip === 2) {
            return item.time;
        }
        if (item.pos.x > bailFactor * m.target.x || item.pos.y > bailFactor * m.target.y) continue;

        const key = JSON.stringify({ x: item.pos.x, y: item.pos.y, equip: item.equip });
        if (distances.has(key) && distances.get(key)! < item.time) continue;

        for (const n of m.neighbors(item.pos, item.equip)) {
            const dKey = JSON.stringify({ x: n.pos.x, y: n.pos.y, equip: n.equip });
            if (!distances.has(dKey) || item.time + n.time < distances.get(dKey)!) {
                distances.set(dKey, item.time + n.time);
                queue.push({ pos: n.pos, time: item.time + n.time, equip: n.equip });
            }
        }
    }
    return 0;
}

const input = fs.readFileSync('input.txt', 'utf-8');
console.log(rescue(input));