
import * as fs from 'fs';

type P = { x: number; y: number };
enum Dir { N, E, S, W }

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

    walk(map: Map<string, boolean>): boolean {
        const dir = Dirs[this.facing];
        let next: P = { x: this.curr.x + dir.x, y: this.curr.y + dir.y };

        if (map.has(`${next.x},${next.y}`)) {
            if (map.get(`${next.x},${next.y}`)) {
                return false;
            }
            this.curr = next;
            return true;
        }

        const oppDir: P = { x: -dir.x, y: -dir.y };
        while (true) {
            const lookAhead: P = { x: next.x + oppDir.x, y: next.y + oppDir.y };
            if (!map.has(`${lookAhead.x},${lookAhead.y}`)) {
                if (map.get(`${next.x},${next.y}`)) {
                    return false;
                }
                this.curr = next;
                return true;
            }
            next = lookAhead;
        }
    }
}

function parse(input: string): { map: Map<string, boolean>, movements: Movement[], size: number } {
    const lines = input.split('\n');
    let size = 0;
    const map = new Map<string, boolean>();

    let r = 0;
    for (; r < lines.length; r++) {
        const line = lines[r];
        if (line === '') break;

        if (r === 0) {
            size = Math.floor(line.length / 3);
        }

        for (let c = 0; c < line.length; c++) {
            const char = line[c];
            if (char === ' ') continue;
            map.set(`${r},${c}`, char === '#');
        }
    }

    const movements = parsePath(lines[r + 1]);
    return { map, movements, size };
}

function parsePath(path: string): Movement[] {
    const movements: Movement[] = [];
    let acc = 0;
    for (const char of path) {
        if (char === 'R' || char === 'L') {
            movements.push({ steps: acc });
            acc = 0;
            movements.push({ rotate: char });
        } else {
            acc = acc * 10 + (char.charCodeAt(0) - '0'.charCodeAt(0));
        }
    }
    movements.push({ steps: acc });
    return movements;
}

function solve() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const { map, movements, size } = parse(input);

    const human = new Human({ x: 0, y: size }, Dir.E);

    for (const mov of movements) {
        if (mov.rotate) {
            human.rotate(mov.rotate);
        } else if (mov.steps) {
            for (let i = 0; i < mov.steps; i++) {
                if (!human.walk(map)) {
                    break;
                }
            }
        }
    }

    console.log(1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.points());
}

solve();
