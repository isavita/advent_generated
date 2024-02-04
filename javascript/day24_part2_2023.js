const fs = require('fs');

class RatVec3 {
	constructor(x, y, z) {
		this.X = BigInt(x);
		this.Y = BigInt(y);
		this.Z = BigInt(z);
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

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
console.log(solve(input));

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
	return (rp.X + rp.Y + rp.Z).toString();
}

function readInput(input) {
	return input.map(readLine);
}

function readLine(line) {
	const match = line.match(/-*\d+/g).map((s) => BigInt(s));

	return new HailStone(
		new RatVec3(match[0], match[1], match[2]),
		new RatVec3(match[3], match[4], match[5])
	);
}

function intersectionTime(r, s) {
	const plane = r.p.Cross(r.p.Add(r.v));
	return (s.p.Dot(plane) * BigInt(-1)) / s.v.Dot(plane);
}