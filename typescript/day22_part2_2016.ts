
import * as fs from 'fs';
import * as path from 'path';

type Point = { x: number; y: number };
type Node = { used: number; avail: number };

const Neighbors4: Point[] = [
    { x: 0, y: 1 },
    { x: 0, y: -1 },
    { x: 1, y: 0 },
    { x: -1, y: 0 },
];

function main(): void {
    const filePath = path.join(__dirname, 'input.txt');
    const inputLines = fs.readFileSync(filePath, 'utf-8').split('\n').slice(2);

    const nodes: { [key: string]: Node } = {};
    for (const line of inputLines) {
        if (!line) continue;
        const fields = line.split(/\s+/);
        const matches = /-x(\d+)-y(\d+)/.exec(fields[0]);
        if (matches) {
            const p: Point = { x: parseInt(matches[1]), y: parseInt(matches[2]) };
            const n: Node = { used: parseInt(fields[2]), avail: parseInt(fields[3]) };
            nodes[`${p.x},${p.y}`] = n;
        }
    }

    console.log(minMoves(nodes));
}

function minMoves(nodes: { [key: string]: Node }): number {
    const { w } = dim(nodes);
    let goal: Point = { x: w, y: 0 };
    let hole: Point = findHole(nodes);
    let movesSum = 0;

    while (goal.x !== 0 || goal.y !== 0) {
        let nextPos: Point = { x: goal.x - 1, y: 0 };
        let m = moves(nodes, goal, hole, nextPos);
        movesSum += m;
        hole = nextPos;
        m = moves(nodes, goal, goal, hole);
        movesSum += m;
        [goal, hole] = [hole, goal];
    }
    return movesSum;
}

function findHole(nodes: { [key: string]: Node }): Point {
    for (const key in nodes) {
        if (nodes.hasOwnProperty(key)) {
            const p = key.split(',').map(Number);
            const point: Point = {x: p[0], y: p[1]};
            if (nodes[key].used === 0) {
                return point;
            }
        }
    }
    throw new Error("No hole found");
}

function moves(nodes: { [key: string]: Node }, goal: Point, fromPos: Point, toPos: Point): number {
    const { w, h } = dim(nodes);
    const depth: { [key: string]: number } = {};
    depth[`${fromPos.x},${fromPos.y}`] = 0;
    const pq: [number, Point][] = [[0, fromPos]];

    while (pq.length > 0) {
        pq.sort((a, b) => a[0] - b[0]);
        const [currDepth, p] = pq.shift()!;
        if (p.x === toPos.x && p.y === toPos.y) {
            return currDepth;
        }

        const currDepthNext = depth[`${p.x},${p.y}`] + 1;
        for (const n of Neighbors4) {
            const nextPos: Point = { x: p.x + n.x, y: p.y + n.y };
            if (
                nextPos.x < 0 ||
                nextPos.y < 0 ||
                nextPos.x > w ||
                nextPos.y > h ||
                nodes[`${nextPos.x},${nextPos.y}`]?.used > 400 ||
                (nextPos.x === goal.x && nextPos.y === goal.y)
            ) {
                continue;
            }

            const nextPosKey = `${nextPos.x},${nextPos.y}`;
            if (!(nextPosKey in depth) || currDepthNext < depth[nextPosKey]) {
                depth[nextPosKey] = currDepthNext;
                pq.push([currDepthNext, nextPos]);
            }
        }
    }

    throw new Error("No possible path");
}

function dim(nodes: { [key: string]: Node }): { w: number; h: number } {
    let w = 0;
    let h = 0;
    for (const key in nodes) {
        if (nodes.hasOwnProperty(key)) {
            const p = key.split(',').map(Number);
            w = Math.max(w, p[0]);
            h = Math.max(h, p[1]);
        }
    }
    return { w, h };
}

main();
