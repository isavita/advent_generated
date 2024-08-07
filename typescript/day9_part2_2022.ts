import * as fs from 'fs';

type Position = [number, number];

const moveTail = (head: Position, tail: Position): Position => {
    const [hx, hy] = head;
    const [tx, ty] = tail;
    if (Math.abs(hx - tx) <= 1 && Math.abs(hy - ty) <= 1) return tail;

    const dx = hx > tx ? 1 : hx < tx ? -1 : 0;
    const dy = hy > ty ? 1 : hy < ty ? -1 : 0;
    return [tx + dx, ty + dy];
};

const simulateRope = (motions: string[]): number => {
    const knots: Position[] = Array(10).fill(null).map(() => [0, 0]);
    const visited = new Set<string>();
    visited.add(knots[0].toString());

    for (const motion of motions) {
        const [direction, steps] = motion.split(' ');
        const stepCount = parseInt(steps);

        for (let step = 0; step < stepCount; step++) {
            const head = knots[0];
            switch (direction) {
                case 'R': head[0]++; break;
                case 'L': head[0]--; break;
                case 'U': head[1]++; break;
                case 'D': head[1]--; break;
            }

            for (let i = 1; i < knots.length; i++) {
                knots[i] = moveTail(knots[i - 1], knots[i]);
            }

            visited.add(knots[knots.length - 1].toString());
        }
    }

    return visited.size;
};

const input = fs.readFileSync('input.txt', 'utf-8');
const motions = input.trim().split('\n');
const result = simulateRope(motions);
console.log(result);