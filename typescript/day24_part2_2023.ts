const fs = require('fs');

const zero = BigInt(0);

class RatVec3 {
	constructor(x, y, z) {
		this.X = x;
		this.Y = y;
		this.Z = z;
	}

	Add(other) {
		return new RatVec3(this.X + other.X, this.Y + other.Y, this.Z + other.Z);
	}

	Subtract(other) {
		return new RatVec3(this.X - other.X, this.Y - other.Y, this.Z - other.Z);
	}

	Multiply(s) {
		return new RatVec3(this.X * s, this.Y * s, this.Z * s);
	}

	Divide(s) {
		return new RatVec3(this.X / s, this.Y / s, this.Z / s);
	}

	Cross(other) {
		return new RatVec3(
			this.Y * other.Z - this.Z * other.Y,
			this.Z * other.X - this.X * other.Z,
			this.X * other.Y - this.Y * other.X
		);
	}

	Dot(other) {
		return this.X * other.X + this.Y * other.Y + this.Z * other.Z;
	}
}

class HailStone {
	constructor(p, v) {
		this.p = p;
		this.v = v;
	}

	Subtract(other) {
		return new HailStone(this.p.Subtract(other.p), this.v.Subtract(other.v));
	}
}

function readFile(fileName) {
	return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

function solve(input) {
	const hailStones = readInput(input.slice(0, 3));
	const s1 = hailStones[1];
	const s2 = hailStones[2];
	const ref1 = s1.Subtract(hailStones[0]);
	const ref2 = s2.Subtract(hailStones[0]);

	const t1 = intersectionTime(ref2, ref1);
	const t2 = intersectionTime(ref1, ref2);

	const rock1 = s1.p.Add(s1.v.Multiply(t1));
	const rock2 = s2.p.Add(s2.v.Multiply(t2));

	const rp = rock1.Subtract(rock2.Subtract(rock1).Divide(t2 - t1).Multiply(t1));
	return rp.X + rp.Y + rp.Z;
}

function readInput(input) {
	return input.map(readLine);
}

function readLine(line) {
	const match = line.match(/-*\d+/g).map(Number);

	return new HailStone(
		new RatVec3(BigInt(match[0]), BigInt(match[1]), BigInt(match[2])),
		new RatVec3(BigInt(match[3]), BigInt(match[4]), BigInt(match[5]))
	);
}

function countIntersections(stones, start, end) {
	let count = 0;

	for (let i = 0; i < stones.length; i++) {
		for (let j = 0; j < stones.length - i - 1; j++) {
			const [x, y, t1, t2, ok] = doMatch2D(stones[i], stones[i + j + 1]);
			if (ok && t1 > zero && t2 >= zero && x >= start && y >= start && x <= end && y <= end) {
				count++;
			}
		}
	}

	return count;
}

function doMatch2D(a, b) {
	const m1 = a.v.Y / a.v.X;
	const m2 = b.v.Y / b.v.X;

	if (m1 === m2) {
		return [null, null, null, null, false];
	}

	const x = (m1 * a.p.X - a.p.Y - m2 * b.p.X + b.p.Y) / (m1 - m2);
	const y = m1 * (x - a.p.X) + a.p.Y;
	const t1 = (x - a.p.X) / a.v.X;
	const t2 = (x - b.p.X) / b.v.X;

	return [x, y, t1, t2, true];
}

function intersectionTime(r, s) {
	const plane = r.p.Cross(r.p.Add(r.v));
	return -s.p.Dot(plane) / s.v.Dot(plane);
}

function add(a, b) {
	return a + b;
}

function sub(a, b) {
	return a - b;
}

function mul(a, b) {
	return a * b;
}

function quo(a, b) {
	return a / b;
}

const input = readFile('input.txt');
console.log(solve(input));