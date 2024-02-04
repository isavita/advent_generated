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
	const height = input.length;
	const width = input[0].length;
	const data = {};

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
	for (const key in grid.data) {
		if (grid.data[key] === Start) {
			const [x, y] = key.split(',').map(Number);
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

	for (const neighbor of neighbors) {
		if (isInBounds(grid, neighbor) && grid.data[`${neighbor.x},${neighbor.y}`] !== Rock) {
			validNeighbors.push(neighbor);
		}
	}

	return validNeighbors;
}

function breadthFirstSearch(grid, start, neighborFunc) {
	const frontier = [start];
	const reached = { [`${start.x},${start.y}`]: true };
	const cameFrom = { [`${start.x},${start.y}`]: start };
	const distances = { [`${start.x},${start.y}`]: 0 };

	while (frontier.length > 0) {
		const current = frontier.shift();

		for (const next of neighborFunc(grid, current)) {
			const key = `${next.x},${next.y}`;
			if (!reached[key]) {
				frontier.push(next);
				reached[key] = true;
				cameFrom[key] = current;
				distances[key] = distances[`${current.x},${current.y}`] + 1;
			}
		}
	}

	return distances;
}

function solve(input, numSteps) {
	const grid = parseInput(input);

	const start = findStart(grid);
	const distances = breadthFirstSearch(grid, start, neighbors4);

	let cnt = 0;
	for (const key in distances) {
		const dist = distances[key];
		if (dist <= numSteps && dist % 2 === 0) {
			cnt++;
		}
	}
	return cnt;
}

function readFile(fileName) {
	const input = fs.readFileSync(fileName, 'utf8').trim().split('\n');
	return input;
}

const input = readFile("input.txt");
console.log(solve(input, 64));