const fs = require('fs');

class Nanobot {
    constructor(x, y, z, radius) {
        this.x = x;
        this.y = y;
        this.z = z;
        this.radius = radius;
    }
}

function parseNanobots(input) {
    const nanobots = [];
    const re = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/;

    input.split('\n').forEach(line => {
        const matches = line.match(re);
        const x = parseInt(matches[1]);
        const y = parseInt(matches[2]);
        const z = parseInt(matches[3]);
        const radius = parseInt(matches[4]);
        nanobots.push(new Nanobot(x, y, z, radius));
    });

    return nanobots;
}

function findStrongestNanobot(nanobots) {
    let strongest = nanobots[0];
    nanobots.forEach(nanobot => {
        if (nanobot.radius > strongest.radius) {
            strongest = nanobot;
        }
    });
    return strongest;
}

function countNanobotsInRange(nanobots, strongest) {
    let count = 0;
    nanobots.forEach(nanobot => {
        if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
            count++;
        }
    });
    return count;
}

function manhattanDistance(a, b) {
    return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z);
}

const input = fs.readFileSync('input.txt', 'utf8');
const nanobots = parseNanobots(input);
const strongest = findStrongestNanobot(nanobots);
const inRangeCount = countNanobotsInRange(nanobots, strongest);

console.log(inRangeCount);