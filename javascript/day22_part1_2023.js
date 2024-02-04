const fs = require('fs');

class Coord {
    constructor(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

class Brick {
    constructor(mini, maxi) {
        this.mini = mini;
        this.maxi = maxi;
        this.basedOn = [];
        this.support = [];
    }
}

function parseInput(input) {
    return input.map(line => {
        const [miniStr, maxiStr] = line.split('~');
        const [miniX, miniY, miniZ] = miniStr.split(',').map(Number);
        const [maxiX, maxiY, maxiZ] = maxiStr.split(',').map(Number);
        return new Brick(new Coord(miniX, miniY, miniZ), new Coord(maxiX, maxiY, maxiZ));
    });
}

function settle(bricks) {
    bricks.sort((a, b) => a.maxi.z - b.maxi.z);

    for (let i = 0; i < bricks.length; i++) {
        let supportZ = 0;
        let basedBricks = [];

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
        basedBricks.forEach(basedBrick => basedBrick.support.push(bricks[i]));

        const deltaZ = bricks[i].maxi.z - bricks[i].mini.z;
        bricks[i].mini.z = supportZ + 1;
        bricks[i].maxi.z = bricks[i].mini.z + deltaZ;
    }
}

function solve(input) {
    const bricks = parseInput(input);
    settle(bricks);

    let cnt = 0;
    bricks.forEach(brick => {
        let isDisintegratable = true;
        brick.support.forEach(supportedBrick => {
            if (supportedBrick.basedOn.length < 2) {
                isDisintegratable = false;
            }
        });
        if (isDisintegratable) {
            cnt++;
        }
    });

    return cnt;
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
console.log(solve(input));