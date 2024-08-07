import * as fs from 'fs';
import * as readline from 'readline';

interface Vec3 {
    x: number;
    y: number;
    z: number;
}

interface Moon {
    pos: Vec3;
    vel: Vec3;
}

function abs(x: number): number {
    return x < 0 ? -x : x;
}

function applyGravity(moons: Moon[]): void {
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

function applyVelocity(moons: Moon[]): void {
    for (const moon of moons) {
        moon.pos.x += moon.vel.x;
        moon.pos.y += moon.vel.y;
        moon.pos.z += moon.vel.z;
    }
}

function totalEnergy(moons: Moon[]): number {
    let total = 0;
    for (const moon of moons) {
        const pot = abs(moon.pos.x) + abs(moon.pos.y) + abs(moon.pos.z);
        const kin = abs(moon.vel.x) + abs(moon.vel.y) + abs(moon.vel.z);
        total += pot * kin;
    }
    return total;
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const moons: Moon[] = [];
    for await (const line of rl) {
        const match = line.match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/);
        if (match) {
            const [, x, y, z] = match.map(Number);
            moons.push({ pos: { x, y, z }, vel: { x: 0, y: 0, z: 0 } });
        }
    }

    for (let step = 0; step < 1000; step++) {
        applyGravity(moons);
        applyVelocity(moons);
    }

    console.log(totalEnergy(moons));
}

main();