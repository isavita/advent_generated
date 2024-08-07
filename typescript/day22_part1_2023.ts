type Coord = {
    x: number;
    y: number;
    z: number;
};

type Brick = {
    mini: Coord;
    maxi: Coord;
    basedOn: Brick[];
    support: Brick[];
};

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

function settle(bricks: Brick[]): void {
    bricks.sort((a, b) => a.maxi.z - b.maxi.z);

    for (let i = 0; i < bricks.length; i++) {
        const brick = bricks[i];
        let supportZ = 0;
        const basedBricks: Brick[] = [];

        for (let j = i - 1; j >= 0; j--) {
            const otherBrick = bricks[j];
            const isIntersectingX = Math.max(brick.mini.x, otherBrick.mini.x) <= Math.min(brick.maxi.x, otherBrick.maxi.x);
            const isIntersectingY = Math.max(brick.mini.y, otherBrick.mini.y) <= Math.min(brick.maxi.y, otherBrick.maxi.y);
            if (isIntersectingX && isIntersectingY) {
                if (otherBrick.maxi.z === supportZ) {
                    basedBricks.push(otherBrick);
                } else if (otherBrick.maxi.z > supportZ) {
                    supportZ = otherBrick.maxi.z;
                    basedBricks.length = 0;
                    basedBricks.push(otherBrick);
                }
            }
        }

        brick.basedOn = basedBricks;
        for (const basedBrick of basedBricks) {
            basedBrick.support.push(brick);
        }

        const deltaZ = brick.maxi.z - brick.mini.z;
        brick.mini.z = supportZ + 1;
        brick.maxi.z = brick.mini.z + deltaZ;
    }
}

function solve(input: string[]): number {
    const bricks = parseInput(input);
    settle(bricks);

    let cnt = 0;
    for (const brick of bricks) {
        let isDisintegratable = true;
        for (const supportedBrick of brick.support) {
            if (supportedBrick.basedOn.length < 2) {
                isDisintegratable = false;
                break;
            }
        }
        if (isDisintegratable) {
            cnt++;
        }
    }
    return cnt;
}

function readFile(fileName: string): string[] {
    const fs = require('fs');
    const fileContent = fs.readFileSync(fileName, 'utf-8');
    return fileContent.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));