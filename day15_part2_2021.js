const fs = require('fs');

class PriorityQueue {
	constructor() {
		this.data = [];
	}

	enqueue(element) {
		this.data.push(element);
		this.data.sort((a, b) => a[2] - b[2]);
	}

	dequeue() {
		return this.data.shift();
	}

	isEmpty() {
		return this.data.length === 0;
	}
}

function dijkstra(grid) {
	const pq = new PriorityQueue();
	pq.enqueue([0, 0, 0]);

	const rows = grid.length;
	const cols = grid[0].length;
	const dist = new Array(rows).fill().map(() => new Array(cols).fill(Infinity));
	dist[0][0] = 0;

	const directions = [[1, 0], [0, 1], [-1, 0], [0, -1]];

	while (!pq.isEmpty()) {
		const [x, y, risk] = pq.dequeue();
		if (x === rows - 1 && y === cols - 1) {
			return risk;
		}
		for (const [dx, dy] of directions) {
			const nx = x + dx;
			const ny = y + dy;
			if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
				const nextRisk = risk + grid[nx][ny];
				if (nextRisk < dist[nx][ny]) {
					dist[nx][ny] = nextRisk;
					pq.enqueue([nx, ny, nextRisk]);
				}
			}
		}
	}
	return -1;
}

function extendGrid(initialGrid) {
	const rows = initialGrid.length;
	const cols = initialGrid[0].length;
	const extendedGrid = new Array(rows * 5).fill().map(() => new Array(cols * 5).fill(0));
	for (let i = 0; i < rows * 5; i++) {
		for (let j = 0; j < cols * 5; j++) {
			const newRisk = initialGrid[i % rows][j % cols] + Math.floor(i / rows) + Math.floor(j / cols);
			extendedGrid[i][j] = newRisk > 9 ? newRisk - 9 : newRisk;
		}
	}
	return extendedGrid;
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.trim().split('').map(Number));
const extendedGrid = extendGrid(input);
console.log(dijkstra(extendedGrid));