
const fs = require('fs');

class Vec3 {
    constructor(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

class Moon {
    constructor(pos, vel) {
        this.pos = pos;
        this.vel = vel;
    }
}

function applyGravity(moons, axis) {
    for (let i = 0; i < moons.length; i++) {
        for (let j = i + 1; j < moons.length; j++) {
            switch (axis) {
                case "x":
                    if (moons[i].pos.x > moons[j].pos.x) {
                        moons[i].vel.x--;
                        moons[j].vel.x++;
                    } else if (moons[i].pos.x < moons[j].pos.x) {
                        moons[i].vel.x++;
                        moons[j].vel.x--;
                    }
                    break;
                case "y":
                    if (moons[i].pos.y > moons[j].pos.y) {
                        moons[i].vel.y--;
                        moons[j].vel.y++;
                    } else if (moons[i].pos.y < moons[j].pos.y) {
                        moons[i].vel.y++;
                        moons[j].vel.y--;
                    }
                    break;
                case "z":
                    if (moons[i].pos.z > moons[j].pos.z) {
                        moons[i].vel.z--;
                        moons[j].vel.z++;
                    } else if (moons[i].pos.z < moons[j].pos.z) {
                        moons[i].vel.z++;
                        moons[j].vel.z--;
                    }
                    break;
            }
        }
    }
}

function applyVelocity(moons, axis) {
    for (let i = 0; i < moons.length; i++) {
        switch (axis) {
            case "x":
                moons[i].pos.x += moons[i].vel.x;
                break;
            case "y":
                moons[i].pos.y += moons[i].vel.y;
                break;
            case "z":
                moons[i].pos.z += moons[i].vel.z;
                break;
        }
    }
}

function findCycle(moons, initialMoons, axis) {
    for (let steps = 1; ; steps++) {
        applyGravity(moons, axis);
        applyVelocity(moons, axis);

        let match = true;
        for (let i = 0; i < moons.length; i++) {
            switch (axis) {
                case "x":
                    if (moons[i].pos.x !== initialMoons[i].pos.x || moons[i].vel.x !== initialMoons[i].vel.x) {
                        match = false;
                    }
                    break;
                case "y":
                    if (moons[i].pos.y !== initialMoons[i].pos.y || moons[i].vel.y !== initialMoons[i].vel.y) {
                        match = false;
                    }
                    break;
                case "z":
                    if (moons[i].pos.z !== initialMoons[i].pos.z || moons[i].vel.z !== initialMoons[i].vel.z) {
                        match = false;
                    }
                    break;
            }
        }

        if (match) {
            return steps;
        }
    }
}

function lcm(a, b) {
    return BigInt(a * b) / BigInt(gcd(a, b));
}

function gcd(a, b) {
    if (b === 0) {
        return a;
    }
    return gcd(b, a % b);
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
let moons = [];
let initialMoons = [];

for (let line of input) {
    const [x, y, z] = line.match(/-?\d+/g).map(Number);
    moons.push(new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0)));
    initialMoons.push(new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0)));
}

const cycleX = findCycle([...moons], [...initialMoons], "x");
const cycleY = findCycle([...moons], [...initialMoons], "y");
const cycleZ = findCycle([...moons], [...initialMoons], "z");

const lcmXY = lcm(cycleX, cycleY);
const lcmXYZ = lcm(Number(lcmXY), cycleZ);

console.log(lcmXYZ.toString());
