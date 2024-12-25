
import * as fs from 'fs';

type Rat = {
    n: bigint;
    d: bigint;
};

const zero: Rat = { n: 0n, d: 1n };

const gcd = (a: bigint, b: bigint): bigint => {
    while (b) {
        [a, b] = [b, a % b];
    }
    return a;
};

const normalize = (rat: Rat): Rat => {
    const g = gcd(rat.n, rat.d);
    return { n: rat.n / g, d: rat.d / g };
};

const add = (a: Rat, b: Rat): Rat => {
    return normalize({ n: a.n * b.d + b.n * a.d, d: a.d * b.d });
};

const sub = (a: Rat, b: Rat): Rat => {
    return normalize({ n: a.n * b.d - b.n * a.d, d: a.d * b.d });
};

const mul = (a: Rat, b: Rat): Rat => {
    return normalize({ n: a.n * b.n, d: a.d * b.d });
};

const quo = (a: Rat, b: Rat): Rat => {
    return normalize({ n: a.n * b.d, d: a.d * b.n });
};

const ratToString = (rat: Rat): string => {
    return `${rat.n}/${rat.d}`;
}

const ratToFloat = (rat: Rat): number => {
    return Number(rat.n) / Number(rat.d);
}

type RatVec3 = {
    x: Rat;
    y: Rat;
    z: Rat;
};

const addVec = (vec: RatVec3, other: RatVec3): RatVec3 => {
    return {
        x: add(vec.x, other.x),
        y: add(vec.y, other.y),
        z: add(vec.z, other.z),
    };
};

const subVec = (vec: RatVec3, other: RatVec3): RatVec3 => {
    return {
        x: sub(vec.x, other.x),
        y: sub(vec.y, other.y),
        z: sub(vec.z, other.z),
    };
};

const mulVec = (vec: RatVec3, s: Rat): RatVec3 => {
    return {
        x: mul(vec.x, s),
        y: mul(vec.y, s),
        z: mul(vec.z, s),
    };
};

const divVec = (vec: RatVec3, s: Rat): RatVec3 => {
    return {
        x: quo(vec.x, s),
        y: quo(vec.y, s),
        z: quo(vec.z, s),
    };
};

const crossVec = (vec: RatVec3, other: RatVec3): RatVec3 => {
    return {
        x: sub(mul(vec.y, other.z), mul(vec.z, other.y)),
        y: sub(mul(vec.z, other.x), mul(vec.x, other.z)),
        z: sub(mul(vec.x, other.y), mul(vec.y, other.x)),
    };
};

const dotVec = (vec: RatVec3, other: RatVec3): Rat => {
    return add(mul(vec.x, other.x), add(mul(vec.y, other.y), mul(vec.z, other.z)));
};

type HailStone = {
    p: RatVec3;
    v: RatVec3;
};

const subHail = (stone: HailStone, other: HailStone): HailStone => {
    return {
        p: subVec(stone.p, other.p),
        v: subVec(stone.v, other.v),
    };
};

const readFile = (fileName: string): string[] => {
    const file = fs.readFileSync(fileName, 'utf-8');
    return file.trim().split('\n');
};

const solve = (input: string[]): string => {
    const hailStones = readInput(input.slice(0, 3));
    const s1 = hailStones[1];
    const s2 = hailStones[2];
    const ref1 = subHail(s1, hailStones[0]);
    const ref2 = subHail(s2, hailStones[0]);

    const t1 = intersectionTime(ref2, ref1);
    const t2 = intersectionTime(ref1, ref2);

    const rock1 = addVec(s1.p, mulVec(s1.v, t1));
    const rock2 = addVec(s2.p, mulVec(s2.v, t2));

    const rp = subVec(rock1, mulVec(divVec(subVec(rock2, rock1), sub(t2, t1)), t1));
    return (ratToFloat(add(rp.x, add(rp.y, rp.z)))).toFixed(0);
};

const readInput = (input: string[]): HailStone[] => {
    return input.map(readLine);
};

const readLine = (line: string): HailStone => {
    const match = line.match(/-?\d+/g)!.map(BigInt);
    return {
        p: {
            x: { n: match[0], d: 1n },
            y: { n: match[1], d: 1n },
            z: { n: match[2], d: 1n },
        },
        v: {
            x: { n: match[3], d: 1n },
            y: { n: match[4], d: 1n },
            z: { n: match[5], d: 1n },
        },
    };
};

const intersectionTime = (r: HailStone, s: HailStone): Rat => {
    const plane = crossVec(r.p, addVec(r.p, r.v));
    return quo(mul(dotVec(s.p, plane), { n: -1n, d: 1n }), dotVec(s.v, plane));
};

const input = readFile('input.txt');
console.log(solve(input));
