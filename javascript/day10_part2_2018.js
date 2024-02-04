const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

class Star {
    constructor(x, y, vX, vY) {
        this.x = x;
        this.y = y;
        this.vX = vX;
        this.vY = vY;
        this.next = null;
    }
}

const head = new Star();
let tail = head;

const re = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/;

for (const line of input) {
    const split = re.exec(line);
    if (split && split.length === 5) {
        const star = new Star(
            parseInt(split[1]),
            parseInt(split[2]),
            parseInt(split[3]),
            parseInt(split[4])
        );
        tail.next = star;
        tail = star;
    }
}

let smallestT = 0;
let smallestArea = Number.MAX_SAFE_INTEGER;

for (let t = 1; t < 100000; t++) {
    let maxX = 0;
    let maxY = 0;
    let minX = 0;
    let minY = 0;

    for (let temp = head.next; temp.next !== null; temp = temp.next) {
        const x = temp.x + temp.vX * t;
        if (maxX < x) {
            maxX = x;
        } else if (minX > x) {
            minX = x;
        }
        const y = temp.y + temp.vY * t;
        if (maxY < y) {
            maxY = y;
        } else if (minY > y) {
            minY = y;
        }
    }

    const lenX = maxX - minY + 1;
    const lenY = maxY - minY + 1;
    const area = lenX + lenY;

    if (smallestArea > area) {
        smallestArea = area;
        smallestT = t;
    }
}

console.log(smallestT);