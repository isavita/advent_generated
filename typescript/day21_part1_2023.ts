const fs = require('fs');

class Coord {
	constructor(x, y) {
		this.x = x;
		this.y = y;
	}

	add(c2) {
		return new Coord(this.x + c2.x, this.y + c2.y);
	}

	multiplyByScalar(s) {
		return new Coord(this.x * s, this.y * s);
	}
}

class Grid {
	constructor(width, height, data) {
		this.width = width;
		this.height = height;
		this.data = data;
	}

	toString() {
		let res = "";
		for (let y = 0; y < this.height; y++) {
			for (let x = 0; x < this.width; x++) {
				if (this.data[`${x},${y}`]) {
					res += this.data[`${x},${y}`];
				} else {
					res += '.';
				}
			}
			res += "\n";
		}
		return res;
	}
}

const North = new Coord(0, -1);
const West = new Coord(-1, 0);
const South = new Coord(0, 1);
const East = new Coord(1, 0);

const Empty = '.';
const Rock = '#';
const Start = 'S';

function isInBounds(grid, coord) {
	return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

function parseInput(input) {
	let data = {};
	let height = input.length;
	let width = input[0].length;

	for (let y = 0; y < height; y++) {
		for (let x = 0; x < width; x++) {
			if (input[y][x] !== Empty) {
				data[`${x},${y}`] = input[y][x];
			}
		}
	}

	return new Grid(width, height, data);
}

function findStart(grid) {
	for (let coordStr in grid.data) {
		if (grid.data[coordStr] === Start) {
			let [x, y] = coordStr.split(',').map(Number);
			return new Coord(x, y);
		}
	}
	throw new Error("No start found.");
}

function neighbors4(grid, coord) {
	const neighbors = [
		coord.add(North),
		coord.add(South),
		coord.add(East),
		coord.add(West),
	];

	const validNeighbors = [];

	for (let neighbor of neighbors) {
		if (isInBounds(grid, neighbor) && grid.data[`${neighbor.x},${neighbor.y}`] !== Rock) {
			validNeighbors.push(neighbor);
		}
	}

	return validNeighbors;
}

function breadthFirstSearch(grid, start, neighborFunc) {
	let frontier = [start];
	let reached = { [`${start.x},${start.y}`]: true };
	let cameFrom = { [`${start.x},${start.y}`]: start };
	let distances = { [`${start.x},${start.y}`]: 0 };

	while (frontier.length > 0) {
		let current = frontier.shift();

		for (let next of neighborFunc(grid, current)) {
			let nextStr = `${next.x},${next.y}`;
			if (!reached[nextStr]) {
				frontier.push(next);
				reached[nextStr] = true;
				cameFrom[nextStr] = current;
				distances[nextStr] = distances[`${current.x},${current.y}`] + 1;
			}
		}
	}

	return distances;
}

function distancesFromExtremities(grid) {
	let distances = {};

	const extremities = [
		new Coord(0, 0),
		new Coord(Math.floor(grid.width / 2), 0),
		new Coord(grid.width, 0),
		new Coord(grid.width, Math.floor(grid.height / 2)),
		new Coord(grid.width, grid.height),
		new Coord(Math.floor(grid.width / 2), grid.height),
		new Coord(0, grid.height),
		new Coord(0, Math.floor(grid.height / 2)),
	];

	for (let start of extremities) {
		distances[`${start.x},${start.y}`] = breadthFirstSearch(grid, start, neighbors4);
	}

	return distances;
}

function neighbors8(grid, coord) {
	const neighbors = [
		coord.add(North),
		coord.add(South),
		coord.add(East),
		coord.add(West),
		coord.add(North).add(East),
		coord.add(North).add(West),
		coord.add(South).add(East),
		coord.add(South).add(West),
	];

	return neighbors;
}

function solve(input, numSteps) {
	const grid = parseInput(input);

	const start = findStart(grid);
	const distances = breadthFirstSearch(grid, start, neighbors4);

	let cnt = 0;
	for (let coordStr in distances) {
		let dist = distances[coordStr];
		let [x, y] = coordStr.split(',').map(Number);
		if (dist <= numSteps && dist % 2 === 0) {
			cnt++;
		}
	}
	return cnt;
}

function readFile(fileName) {
	try {
		const data = fs.readFileSync(fileName, 'utf8');
		return data.trim().split('\n');
	} catch (err) {
		throw err;
	}
}

const input = readFile("input.txt");
console.log(solve(input, 64));