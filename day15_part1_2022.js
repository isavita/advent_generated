const fs = require('fs');

function abs(n) {
    return n < 0 ? -n : n;
}

function manhattan(p, q) {
    return abs(p.x - q.x) + abs(p.y - q.y);
}

function setOf(a) {
    const s = new Set();
    for (const c of a) {
        s.add(c);
    }
    return s;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').split('\n');
    const sensors = [];

    for (const line of input) {
        const parts = line.match(/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/);
        const s = {
            pos: { x: parseInt(parts[1]), y: parseInt(parts[2]) },
            beacon: { x: parseInt(parts[3]), y: parseInt(parts[4]) }
        };
        s.dist = manhattan(s.pos, s.beacon);
        sensors.push(s);
    }

    console.log(impossible(sensors, 2000000));
}

function impossible(sensors, y) {
    const pts = setOf([]);
    for (const s of sensors) {
        const dist = s.dist - abs(s.pos.y - y);
        for (let x = 0; x <= dist; x++) {
            pts.add(s.pos.x + x);
            pts.add(s.pos.x - x);
        }
    }
    for (const s of sensors) {
        if (s.beacon.y === y) {
            pts.delete(s.beacon.x);
        }
    }
    return pts.size;
}

main();