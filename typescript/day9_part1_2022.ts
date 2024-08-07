import * as fs from 'fs';

type Position = [number, number];

const moveTail = (head: Position, tail: Position): Position => {
    const [hx, hy] = head;
    const [tx, ty] = tail;

    if (Math.abs(hx - tx) <= 1 && Math.abs(hy - ty) <= 1) return tail;

    const newTx = tx + Math.sign(hx - tx);
    const newTy = ty + Math.sign(hy - ty);
    return [newTx, newTy];
};

const simulateRopeBridge = (movements: string[]): number => {
    const head: Position = [0, 0];
    let tail: Position = [0, 0];
    const visited: Set<string> = new Set();
    visited.add(tail.toString());

    for (const move of movements) {
        const [direction, stepsStr] = move.split(' ');
        const steps = parseInt(stepsStr);

        for (let i = 0; i < steps; i++) {
            switch (direction) {
                case 'R': head[0]++; break;
                case 'L': head[0]--; break;
                case 'U': head[1]++; break;
                case 'D': head[1]--; break;
            }
            tail = moveTail(head, tail);
            visited.add(tail.toString());
        }
    }

    return visited.size;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf8');
    const movements = input.trim().split('\n');
    const result = simulateRopeBridge(movements);
    console.log(result);
};

main();