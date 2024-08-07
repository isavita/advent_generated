import * as fs from 'fs';

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const result = solve(input);
    console.log(result);
}

function part1(input: string): number {
    const cubes = parseInput(input);
    const onCoords: { [key: string]: boolean } = {};

    for (const c of cubes) {
        if (part1OutOfBounds(c.x1, c.x2, c.y1, c.y2, c.z1, c.z2)) {
            continue;
        }

        for (let x = c.x1; x <= c.x2; x++) {
            for (let y = c.y1; y <= c.y2; y++) {
                for (let z = c.z1; z <= c.z2; z++) {
                    const coord = `${x},${y},${z}`;
                    onCoords[coord] = c.isOn;
                }
            }
        }
    }

    let count = 0;
    for (const b of Object.values(onCoords)) {
        if (b) {
            count++;
        }
    }

    return count;
}

function part1OutOfBounds(...nums: number[]): boolean {
    return nums.some(n => n < -50 || n > 50);
}

function solve(input: string): number {
    const cubes = parseInput(input);
    let finalList: Cube[] = [];

    for (const c of cubes) {
        let toAdd: Cube[] = [];

        for (const finalCube of finalList) {
            const [intersection, didIntersect] = finalCube.getIntersection(c);
            if (didIntersect) {
                toAdd.push(intersection);
            }
        }

        if (c.isOn) {
            toAdd.push(c);
        }

        finalList.push(...toAdd);
    }

    let total = 0;
    for (const c of finalList) {
        total += c.volume();
    }

    return total;
}

class Cube {
    isOn: boolean;
    x1: number;
    x2: number;
    y1: number;
    y2: number;
    z1: number;
    z2: number;

    constructor(isOn: boolean, x1: number, x2: number, y1: number, y2: number, z1: number, z2: number) {
        this.isOn = isOn;
        this.x1 = x1;
        this.x2 = x2;
        this.y1 = y1;
        this.y2 = y2;
        this.z1 = z1;
        this.z2 = z2;
    }

    getIntersection(c2: Cube): [Cube, boolean] {
        const x1 = Math.max(this.x1, c2.x1);
        const x2 = Math.min(this.x2, c2.x2);
        const y1 = Math.max(this.y1, c2.y1);
        const y2 = Math.min(this.y2, c2.y2);
        const z1 = Math.max(this.z1, c2.z1);
        const z2 = Math.min(this.z2, c2.z2);

        if (x1 > x2 || y1 > y2 || z1 > z2) {
            return [new Cube(false, 0, 0, 0, 0, 0, 0), false];
        }

        const intersectionState = (this.isOn && c2.isOn) ? false : (!this.isOn && !c2.isOn) ? true : c2.isOn;

        return [new Cube(intersectionState, x1, x2, y1, y2, z1, z2), true];
    }

    volume(): number {
        const vol = (this.x2 - this.x1 + 1) * (this.y2 - this.y1 + 1) * (this.z2 - this.z1 + 1);
        return this.isOn ? vol : -vol;
    }
}

function parseInput(input: string): Cube[] {
    const ans: Cube[] = [];
    for (const line of input.split('\n')) {
        const parts = line.split(' ');
        const [x1, x2, y1, y2, z1, z2] = parts[1].match(/-?\d+/g)!.map(Number);

        if (x1 > x2 || y1 > y2 || z1 > z2) {
            throw new Error('Invalid input coordinates');
        }

        ans.push(new Cube(parts[0] === 'on', x1, x2, y1, y2, z1, z2));
    }
    return ans;
}

main();