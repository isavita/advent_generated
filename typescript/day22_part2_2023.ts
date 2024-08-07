import * as fs from 'fs';

interface Coord {
    x: number;
    y: number;
    z: number;
}

interface Brick {
    mini: Coord;
    maxi: Coord;
    basedOn: Brick[];
    support: Brick[];
}

function parseInput(input: string[]): Brick[] {
    return input.map(line => {
        const [x1, y1, z1, x2, y2, z2] = line.split(/[~,]/).map(Number);
        return {
            mini: { x: x1, y: y1, z: z1 },
            maxi: { x: x2, y: y2, z: z2 },
            basedOn: [],
            support: []
        };
    });
}

function settle(bricks: Brick[]) {
    bricks.sort((a, b) => a.maxi.z - b.maxi.z);

    for (let i = 0; i < bricks.length; i++) {
        let supportZ = 0;
        let basedBricks: Brick[] = [];

        for (let j = i - 1; j >= 0; j--) {
            const isIntersectingX = Math.max(bricks[i].mini.x, bricks[j].mini.x) <= Math.min(bricks[i].maxi.x, bricks[j].maxi.x);
            const isIntersectingY = Math.max(bricks[i].mini.y, bricks[j].mini.y) <= Math.min(bricks[i].maxi.y, bricks[j].maxi.y);
            const isIntersecting = isIntersectingX && isIntersectingY;
            if (isIntersecting) {
                if (bricks[j].maxi.z === supportZ) {
                    basedBricks.push(bricks[j]);
                } else if (bricks[j].maxi.z > supportZ) {
                    supportZ = bricks[j].maxi.z;
                    basedBricks = [bricks[j]];
                }
            }
        }

        bricks[i].basedOn = basedBricks;
        for (const basedBrick of basedBricks) {
            basedBrick.support.push(bricks[i]);
        }

        const deltaZ = bricks[i].maxi.z - bricks[i].mini.z;
        bricks[i].mini.z = supportZ + 1;
        bricks[i].maxi.z = bricks[i].mini.z + deltaZ;
    }
}

function solve(input: string[]): number {
    const bricks = parseInput(input);
    settle(bricks);

    let cnt = 0;
    for (const brick of bricks) {
        const fallingBricks = new Set<Brick>();
        for (const supportedBrick of brick.support) {
            if (supportedBrick.basedOn.length === 1) {
                let allSupportedBricks = [supportedBrick];
                while (allSupportedBricks.length > 0) {
                    const supportedBrick0 = allSupportedBricks.shift()!;

                    let isFalling = true;
                    for (const basedBrick of supportedBrick0.basedOn) {
                        if (basedBrick !== brick && !fallingBricks.has(basedBrick)) {
                            isFalling = false;
                            break;
                        }
                    }

                    if (isFalling) {
                        fallingBricks.add(supportedBrick0);
                        allSupportedBricks.push(...supportedBrick0.support);
                    }
                }
            }
        }
        cnt += fallingBricks.size;
    }
    return cnt;
}

function readFile(fileName: string): string[] {
    return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile("input.txt");
console.log(solve(input));