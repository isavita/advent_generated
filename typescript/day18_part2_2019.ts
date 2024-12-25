
import * as fs from 'fs';

type Point = { x: number; y: number };
type Item = { obj: Step; priority: number };
type Grid = Map<string, string>;
type Keys = Set<string>;

const neighbors4: Point[] = [
    { x: 0, y: 1 },
    { x: 0, y: -1 },
    { x: 1, y: 0 },
    { x: -1, y: 0 },
];

function pointToString(p: Point): string {
    return `${p.x},${p.y}`;
}

function parse(input: string): [Grid, Keys, Point] {
    const grid: Grid = new Map();
    const keys: Keys = new Set();
    let start: Point = { x: 0, y: 0 };
    const lines = input.split('\n');
    for (let y = 0; y < lines.length; y++) {
        const line = lines[y];
        for (let x = 0; x < line.length; x++) {
            const char = line[x];
            const p = { x, y };
            grid.set(pointToString(p), char);
            if (char >= 'a' && char <= 'z') {
                keys.add(char);
            } else if (char === '@') {
                start = p;
            }
        }
    }
    return [grid, keys, start];
}

class Step {
    p: Point[];
    workerId: number;
    keys: Keys;

    constructor(p: Point[], workerId: number, keys: Keys) {
        this.p = p;
        this.workerId = workerId;
        this.keys = new Set(keys);
    }

    clone(): Step {
        return new Step([...this.p], this.workerId, this.keys);
    }

    toString(): string {
        const sortedKeys = Array.from(this.keys).sort().join('');
        return `${this.p.map(pointToString).join('_')}_${this.workerId}_${sortedKeys}`;
    }
}

class PriorityQueue {
    private heap: Item[] = [];

    push(item: Item) {
        this.heap.push(item);
        this.heapifyUp();
    }

    pop(): Item | undefined {
        if (this.isEmpty()) {
            return undefined;
        }
        this.swap(0, this.heap.length - 1);
        const item = this.heap.pop();
        this.heapifyDown();
        return item;
    }

    isEmpty(): boolean {
        return this.heap.length === 0;
    }

    private heapifyUp() {
        let index = this.heap.length - 1;
        while (index > 0) {
            const parentIndex = Math.floor((index - 1) / 2);
            if (this.heap[parentIndex].priority >= this.heap[index].priority) {
                break;
            }
            this.swap(index, parentIndex);
            index = parentIndex;
        }
    }

    private heapifyDown() {
        let index = 0;
        while (true) {
            let largest = index;
            const leftChildIndex = 2 * index + 1;
            const rightChildIndex = 2 * index + 2;

            if (leftChildIndex < this.heap.length && this.heap[leftChildIndex].priority > this.heap[largest].priority) {
                largest = leftChildIndex;
            }
            if (rightChildIndex < this.heap.length && this.heap[rightChildIndex].priority > this.heap[largest].priority) {
                largest = rightChildIndex;
            }
            if (largest === index) {
                break;
            }
            this.swap(index, largest);
            index = largest;
        }
    }

    private swap(i: number, j: number) {
        [this.heap[i], this.heap[j]] = [this.heap[j], this.heap[i]];
    }
}

function optimalPath(grid: Grid, keys: Keys, start: Point[]): number {
    const pq = new PriorityQueue();
    const dist: Map<string, number> = new Map();

    const s = new Step(start, 0, keys);
    for (let i = 0; i < start.length; i++) {
        const ss = s.clone();
        ss.workerId = i;
        pq.push({ obj: ss, priority: 0 });
        dist.set(ss.toString(), 0);
    }

    while (!pq.isEmpty()) {
        const currItem = pq.pop()!;
        const curr = currItem.obj;
        const currdist = dist.get(curr.toString())!;
        if (curr.keys.size === 0) {
            return currdist;
        }
        const nextdist = 1 + currdist;
        for (const n of neighbors4) {
            const nextPoint = { x: curr.p[curr.workerId].x + n.x, y: curr.p[curr.workerId].y + n.y };
            const b = grid.get(pointToString(nextPoint));
            if (!b || b === '#') {
                continue;
            }
            if (b >= 'A' && b <= 'Z') {
                if (curr.keys.has(String.fromCharCode(b.charCodeAt(0) + ('a'.charCodeAt(0) - 'A'.charCodeAt(0))))) {
                    continue;
                }
            }
            const next = curr.clone();
            next.p[next.workerId] = nextPoint;
            let foundNewKey = false;
            if (b >= 'a' && b <= 'z') {
                if (next.keys.has(b)) {
                    foundNewKey = true;
                    next.keys.delete(b);
                }
            }
            for (let i = 0; i < next.p.length; i++) {
                if (curr.workerId !== i && !foundNewKey) {
                    continue;
                }
                const worker = next.clone();
                worker.workerId = i;
                const workerKey = worker.toString();
                if (!dist.has(workerKey) || nextdist < dist.get(workerKey)!) {
                    dist.set(workerKey, nextdist);
                    pq.push({ obj: worker, priority: -nextdist });
                }
            }
        }
    }
    throw new Error("No viable path");
}

function readAll(filepath: string): string {
    return fs.readFileSync(filepath, 'utf-8');
}

function main() {
    const [grid, keys, start] = parse(readAll("input.txt"));

    grid.set(pointToString(start), '#');
    for (const n of neighbors4) {
        grid.set(pointToString({ x: start.x + n.x, y: start.y + n.y }), '#');
    }
    const starts: Point[] = [];
    for (const n of [{ x: -1, y: -1 }, { x: -1, y: 1 }, { x: 1, y: -1 }, { x: 1, y: 1 }]) {
        const newStart = { x: start.x + n.x, y: start.y + n.y };
        grid.set(pointToString(newStart), '@');
        starts.push(newStart);
    }
    console.log(optimalPath(grid, keys, starts));
}

main();
