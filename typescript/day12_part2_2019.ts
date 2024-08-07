import * as fs from 'fs';

class Vec3 {
    constructor(public x: number, public y: number, public z: number) {}
}

class Moon {
    constructor(public pos: Vec3, public vel: Vec3) {}
}

function applyGravity(moons: Moon[], axis: 'x' | 'y' | 'z') {
    for (let i = 0; i < moons.length; i++) {
        for (let j = i + 1; j < moons.length; j++) {
            if (moons[i].pos[axis] > moons[j].pos[axis]) {
                moons[i].vel[axis]--;
                moons[j].vel[axis]++;
            } else if (moons[i].pos[axis] < moons[j].pos[axis]) {
                moons[i].vel[axis]++;
                moons[j].vel[axis]--;
            }
        }
    }
}

function applyVelocity(moons: Moon[], axis: 'x' | 'y' | 'z') {
    for (const moon of moons) {
        moon.pos[axis] += moon.vel[axis];
    }
}

function findCycle(moons: Moon[], initialMoons: Moon[], axis: 'x' | 'y' | 'z'): number {
    for (let steps = 1; ; steps++) {
        applyGravity(moons, axis);
        applyVelocity(moons, axis);

        if (moons.every((m, i) => m.pos[axis] === initialMoons[i].pos[axis] && m.vel[axis] === initialMoons[i].vel[axis])) {
            return steps;
        }
    }
}

function gcd(a: number, b: number): number {
    return b === 0 ? a : gcd(b, a % b);
}

function lcm(a: number, b: number): number {
    return (a * b) / gcd(a, b);
}

const data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const moons: Moon[] = [];
const initialMoons: Moon[] = [];

for (const line of data) {
    const [x, y, z] = line.match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/)!.slice(1).map(Number);
    moons.push(new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0)));
    initialMoons.push(new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0)));
}

const cycleX = findCycle(moons, initialMoons, 'x');
const cycleY = findCycle(moons, initialMoons, 'y');
const cycleZ = findCycle(moons, initialMoons, 'z');

const lcmXY = lcm(cycleX, cycleY);
const lcmXYZ = lcm(lcmXY, cycleZ);

console.log(lcmXYZ);