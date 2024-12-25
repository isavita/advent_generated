
import * as fs from 'node:fs';

type P = { x: number; y: number };
enum Dir {
    N, E, S, W
}

const Dirs: P[] = [
    { x: -1, y: 0 },
    { x: 0, y: 1 },
    { x: 1, y: 0 },
    { x: 0, y: -1 },
];

type Movement = { steps?: number; rotate?: 'R' | 'L' };

class Human {
    curr: P;
    facing: Dir;

    constructor(curr: P, facing: Dir) {
        this.curr = curr;
        this.facing = facing;
    }

    rotate(direction: 'R' | 'L'): void {
        this.facing = (direction === 'R' ? (this.facing + 1) : (this.facing - 1 + 4)) % 4;
    }

    points(): number {
        return (this.facing + 3) % 4;
    }

    walk(map: Map<string, boolean>, size: number): boolean {
        const dirDelta = Dirs[this.facing];
        let next: P = { x: this.curr.x + dirDelta.x, y: this.curr.y + dirDelta.y };
        const nextKey = `${next.x},${next.y}`;

        if (map.has(nextKey)) {
            if (map.get(nextKey)) {
                return false;
            }
            this.curr = next;
            return true;
        }

        const [newNext, newFacing] = this.crossBorder(next, size);

        if (map.get(`${newNext.x},${newNext.y}`)) {
            return false;
        }

        this.curr = newNext;
        this.facing = newFacing;
        return true;
    }

    crossBorder(n: P, size: number): [P, Dir] {
        let { x, y } = n;
        let dir = this.facing;

        if (x === -1 && y < 2 * size) {
            return [{ x: y + 2 * size, y: x + 1 }, Dir.E];
        }
        if (x === -1 && y >= 2 * size) {
            return [{ x: x + 4 * size, y: y - 2 * size }, Dir.N];
        }
        if (x === size && dir === Dir.S) {
            return [{ x: y - size, y: x + size - 1 }, Dir.W];
        }
        if (x === 2 * size - 1 && dir === Dir.N) {
            return [{ x: y + size, y: x - size + 1 }, Dir.E];
        }
        if (x === 3 * size && dir === Dir.S) {
            return [{ x: y + 2 * size, y: x - 2 * size - 1 }, Dir.W];
        }
        if (x === 4 * size) {
            return [{ x: x - 4 * size, y: y + 2 * size }, Dir.S];
        }
        if (y === -1 && x < 3 * size) {
            return [{ x: 3 * size - 1 - x, y: y + size + 1 }, Dir.E];
        }
        if (y === -1 && x >= 3 * size) {
            return [{ x: y + 1, y: x - 2 * size }, Dir.S];
        }
        if (y === size - 1 && x < size) {
            return [{ x: 3 * size - 1 - x, y: y - size + 1 }, Dir.E];
        }
        if (y === size - 1 && x >= size && dir === Dir.W) {
            return [{ x: y + size + 1, y: x - size }, Dir.S];
        }
        if (y === size && dir === Dir.E) {
            return [{ x: y + 2 * size - 1, y: x - 2 * size }, Dir.N];
        }
        if (y === 2 * size && x < 2 * size && dir === Dir.E) {
            return [{ x: y - size - 1, y: x + size }, Dir.N];
        }
        if (y === 2 * size && x >= 2 * size) {
            return [{ x: 3 * size - 1 - x, y: y + size - 1 }, Dir.W];
        }
        if (y === 3 * size) {
            return [{ x: 3 * size - 1 - x, y: y - size - 1 }, Dir.W];
        }

        throw new Error("not a border crossing");
    }
}

function parse(data: string): [Map<string, boolean>, number, Movement[]] {
    const lines = data.split('\n');
    const map = new Map<string, boolean>();
    let size = 0;
    let r = 0;

    for (; r < lines.length; r++) {
        const line = lines[r];
        if (line === "") {
            break;
        }

        if (r === 0) {
            size = Math.floor(line.length / 3);
        }

        for (let c = 0; c < line.length; c++) {
            const char = line[c];
            if (char === ' ') continue;
            map.set(`${r},${c}`, char === '#');
        }
    }

    const path = lines[r + 1];
    const movements = parsePath(path);
    return [map, size, movements];
}

function parsePath(path: string): Movement[] {
    const movements: Movement[] = [];
    let acc = 0;
    for (const char of path) {
        if (char === 'R') {
            movements.push({ steps: acc });
            acc = 0;
            movements.push({ rotate: 'R' });
        } else if (char === 'L') {
            movements.push({ steps: acc });
            acc = 0;
            movements.push({ rotate: 'L' });
        } else {
            acc = 10 * acc + (char.charCodeAt(0) - '0'.charCodeAt(0));
        }
    }
    movements.push({ steps: acc });
    return movements;
}

function main() {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const [map, size, movements] = parse(data);

    const human = new Human({ x: 0, y: size }, Dir.E);

    for (const mov of movements) {
        if (mov.rotate) {
            human.rotate(mov.rotate);
        } else if (mov.steps) {
            for (let i = 0; i < mov.steps; i++) {
                if (!human.walk(map, size)) {
                    break;
                }
            }
        }
    }

    console.log(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.points());
}

main();
