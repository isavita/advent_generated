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

function Abs(x) {
    return x < 0 ? -x : x;
}

function applyGravity(moons) {
    for (let i = 0; i < moons.length; i++) {
        for (let j = i + 1; j < moons.length; j++) {
            if (moons[i].pos.x > moons[j].pos.x) {
                moons[i].vel.x--;
                moons[j].vel.x++;
            } else if (moons[i].pos.x < moons[j].pos.x) {
                moons[i].vel.x++;
                moons[j].vel.x--;
            }

            if (moons[i].pos.y > moons[j].pos.y) {
                moons[i].vel.y--;
                moons[j].vel.y++;
            } else if (moons[i].pos.y < moons[j].pos.y) {
                moons[i].vel.y++;
                moons[j].vel.y--;
            }

            if (moons[i].pos.z > moons[j].pos.z) {
                moons[i].vel.z--;
                moons[j].vel.z++;
            } else if (moons[i].pos.z < moons[j].pos.z) {
                moons[i].vel.z++;
                moons[j].vel.z--;
            }
        }
    }
}

function applyVelocity(moons) {
    for (let i = 0; i < moons.length; i++) {
        moons[i].pos.x += moons[i].vel.x;
        moons[i].pos.y += moons[i].vel.y;
        moons[i].pos.z += moons[i].vel.z;
    }
}

function totalEnergy(moons) {
    let total = 0;
    for (let i = 0; i < moons.length; i++) {
        const pot = Abs(moons[i].pos.x) + Abs(moons[i].pos.y) + Abs(moons[i].pos.z);
        const kin = Abs(moons[i].vel.x) + Abs(moons[i].vel.y) + Abs(moons[i].vel.z);
        total += pot * kin;
    }
    return total;
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const moons = input.map(line => {
    const [x, y, z] = line.match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/).slice(1).map(Number);
    return new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0));
});

for (let step = 0; step < 1000; step++) {
    applyGravity(moons);
    applyVelocity(moons);
}

console.log(totalEnergy(moons));