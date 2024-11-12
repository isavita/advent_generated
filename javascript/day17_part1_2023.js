// clumsyCrucible.js

const fs = require('fs');
const path = require('path');

/**
 * Priority Queue implementation using a Min Heap.
 */
class PriorityQueue {
    constructor() {
        this.heap = [];
    }

    /**
     * Inserts an element into the priority queue.
     * @param {Object} element 
     */
    enqueue(element) {
        this.heap.push(element);
        this.bubbleUp(this.heap.length - 1);
    }

    /**
     * Removes and returns the element with the highest priority (lowest totalCost).
     */
    dequeue() {
        if (this.heap.length === 0) return null;
        const min = this.heap[0];
        const end = this.heap.pop();
        if (this.heap.length > 0) {
            this.heap[0] = end;
            this.bubbleDown(0);
        }
        return min;
    }

    /**
     * Returns the number of elements in the priority queue.
     */
    size() {
        return this.heap.length;
    }

    /**
     * Maintains the heap property by moving the element at index up.
     * @param {number} index 
     */
    bubbleUp(index) {
        const element = this.heap[index];
        while (index > 0) {
            const parentIndex = Math.floor((index - 1) / 2);
            const parent = this.heap[parentIndex];
            if (element.totalCost >= parent.totalCost) break;
            this.heap[parentIndex] = element;
            this.heap[index] = parent;
            index = parentIndex;
        }
    }

    /**
     * Maintains the heap property by moving the element at index down.
     * @param {number} index 
     */
    bubbleDown(index) {
        const length = this.heap.length;
        const element = this.heap[index];

        while (true) {
            let leftChildIdx = 2 * index + 1;
            let rightChildIdx = 2 * index + 2;
            let swap = null;

            if (leftChildIdx < length) {
                let leftChild = this.heap[leftChildIdx];
                if (leftChild.totalCost < element.totalCost) {
                    swap = leftChildIdx;
                }
            }

            if (rightChildIdx < length) {
                let rightChild = this.heap[rightChildIdx];
                if (
                    (swap === null && rightChild.totalCost < element.totalCost) ||
                    (swap !== null && rightChild.totalCost < this.heap[swap].totalCost)
                ) {
                    swap = rightChildIdx;
                }
            }

            if (swap === null) break;

            this.heap[index] = this.heap[swap];
            this.heap[swap] = element;
            index = swap;
        }
    }
}

/**
 * Directions and their corresponding (dx, dy) movements.
 */
const DIRECTIONS = {
    'up': { dx: 0, dy: -1 },
    'down': { dx: 0, dy: 1 },
    'left': { dx: -1, dy: 0 },
    'right': { dx: 1, dy: 0 },
};

/**
 * Mirror reflection mappings based on the incoming direction.
 */
const MIRROR_REFLECTION = {
    '/': {
        'up': 'right',
        'right': 'up',
        'down': 'left',
        'left': 'down',
    },
    '\\': {
        'up': 'left',
        'left': 'up',
        'down': 'right',
        'right': 'down',
    },
};

/**
 * Function to determine left and right turns from a given direction.
 */
function getTurnDirections(currentDirection) {
    const directionsOrder = ['up', 'right', 'down', 'left'];
    const idx = directionsOrder.indexOf(currentDirection);
    const leftIdx = (idx + 3) % 4; // Turn left
    const rightIdx = (idx + 1) % 4; // Turn right
    return {
        'left': directionsOrder[leftIdx],
        'right': directionsOrder[rightIdx],
    };
}

/**
 * Reads and parses the grid from input.txt.
 * Returns a 2D array of integers representing heat loss.
 */
function readGrid() {
    const inputPath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(inputPath, 'utf-8');
    const lines = input.trim().split('\n');
    const grid = lines.map(line => line.split('').map(char => parseInt(char, 10)));
    return grid;
}

/**
 * Implements Dijkstra's algorithm to find the least heat loss path.
 * @param {Array} grid 
 * @returns {number} Least total heat loss.
 */
function findLeastHeatLoss(grid) {
    const height = grid.length;
    const width = grid[0].length;

    // Priority Queue initialization
    const pq = new PriorityQueue();

    // Visited Map: key -> `${x},${y},${direction},${consecutive}`
    const visited = new Map();

    // Starting positions: from (0,0), possible directions: 'right' and 'down'
    const startDirections = ['right', 'down'];
    startDirections.forEach(dir => {
        const { dx, dy } = DIRECTIONS[dir];
        const nextX = 0 + dx;
        const nextY = 0 + dy;

        // Check if next position is within grid
        if (nextX >= 0 && nextX < width && nextY >= 0 && nextY < height) {
            const heatLoss = grid[nextY][nextX];
            const stateKey = `${nextX},${nextY},${dir},1`;
            pq.enqueue({
                x: nextX,
                y: nextY,
                direction: dir,
                consecutive: 1,
                totalCost: heatLoss,
            });
            visited.set(stateKey, heatLoss);
        }
    });

    while (pq.size() > 0) {
        const current = pq.dequeue();
        const { x, y, direction, consecutive, totalCost } = current;

        // Check if destination is reached
        if (x === width - 1 && y === height - 1) {
            return totalCost;
        }

        // Determine possible next moves
        const turnDirections = getTurnDirections(direction);
        const possibleMoves = [];

        // Continue straight if consecutive < 3
        if (consecutive < 3) {
            possibleMoves.push({
                direction: direction,
                consecutive: consecutive + 1,
            });
        }

        // Turn left
        possibleMoves.push({
            direction: turnDirections.left,
            consecutive: 1,
        });

        // Turn right
        possibleMoves.push({
            direction: turnDirections.right,
            consecutive: 1,
        });

        // Explore all possible moves
        possibleMoves.forEach(move => {
            const { direction: newDir, consecutive: newConsec } = move;
            const { dx, dy } = DIRECTIONS[newDir];
            const newX = x + dx;
            const newY = y + dy;

            // Check bounds
            if (newX < 0 || newX >= width || newY < 0 || newY >= height) return;

            const heatLoss = grid[newY][newX];
            const newTotalCost = totalCost + heatLoss;
            const stateKey = `${newX},${newY},${newDir},${newConsec}`;

            // Check if this state has been visited with a lower or equal cost
            if (visited.has(stateKey)) {
                if (visited.get(stateKey) <= newTotalCost) return;
            }

            // Enqueue the new state
            pq.enqueue({
                x: newX,
                y: newY,
                direction: newDir,
                consecutive: newConsec,
                totalCost: newTotalCost,
            });
            visited.set(stateKey, newTotalCost);
        });
    }

    // If destination is not reachable
    return -1;
}

// Main Execution
function main() {
    const grid = readGrid();
    const leastHeatLoss = findLeastHeatLoss(grid);
    if (leastHeatLoss !== -1) {
        console.log(leastHeatLoss);
    } else {
        console.log("No path found from start to destination.");
    }
}

main();
