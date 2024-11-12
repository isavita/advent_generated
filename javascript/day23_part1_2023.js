// longestHike.js

const fs = require('fs');
const path = require('path');

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
 * Function to map slope symbols to their respective directions.
 */
const SLOPE_DIRECTIONS = {
    '^': 'up',
    'v': 'down',
    '<': 'left',
    '>': 'right',
};

/**
 * Reads and parses the grid from input.txt.
 * Returns a 2D array of characters representing the grid.
 */
function readGrid() {
    const inputPath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(inputPath, 'utf-8');
    const lines = input.trim().split('\n');
    const grid = lines.map(line => line.split(''));
    return grid;
}

/**
 * Finds the start and end positions on the grid.
 * Start: Single path tile in the top row.
 * End: Single path tile in the bottom row.
 * Returns an object with start and end coordinates.
 */
function findStartEnd(grid) {
    const height = grid.length;
    const width = grid[0].length;

    let start = null;
    let end = null;

    // Find start in top row (y=0)
    for (let x = 0; x < width; x++) {
        if (grid[0][x] === '.') {
            start = { x, y: 0 };
            break;
        }
    }

    // Find end in bottom row (y=height-1)
    for (let x = 0; x < width; x++) {
        if (grid[height - 1][x] === '.') {
            end = { x, y: height - 1 };
            break;
        }
    }

    if (!start || !end) {
        throw new Error("Start or End position not found on the grid.");
    }

    return { start, end };
}

/**
 * Implements Depth-First Search to find the longest hike.
 * @param {Array} grid - 2D array representing the grid.
 * @param {Object} start - Starting position {x, y}.
 * @param {Object} end - Ending position {x, y}.
 * @returns {number} Length of the longest hike.
 */
function findLongestHike(grid, start, end) {
    const height = grid.length;
    const width = grid[0].length;
    let maxSteps = 0;

    /**
     * Recursive DFS function.
     * @param {number} x - Current x-coordinate.
     * @param {number} y - Current y-coordinate.
     * @param {Array} visited - 2D array tracking visited tiles.
     * @param {string|null} directionConstraint - Direction constraint for next move (if any).
     * @param {number} steps - Number of steps taken so far.
     */
    function dfs(x, y, visited, directionConstraint, steps) {
        // If reached the end, update maxSteps
        if (x === end.x && y === end.y) {
            if (steps > maxSteps) {
                maxSteps = steps;
            }
            return;
        }

        // Determine possible moves
        let possibleDirections = [];

        if (directionConstraint) {
            // Must move in the constrained direction
            possibleDirections.push(directionConstraint);
        } else {
            // Can move in any of the four directions
            possibleDirections = ['up', 'down', 'left', 'right'];
        }

        for (const dir of possibleDirections) {
            const { dx, dy } = DIRECTIONS[dir];
            const newX = x + dx;
            const newY = y + dy;

            // Check bounds
            if (newX < 0 || newX >= width || newY < 0 || newY >= height) {
                continue; // Out of bounds
            }

            const tile = grid[newY][newX];

            // Check if tile is traversable
            if (tile === '#' ) {
                continue; // Forest, impassable
            }

            // Check if tile has been visited
            if (visited[newY][newX]) {
                continue; // Already visited
            }

            // Determine if stepping onto a slope
            let newDirectionConstraint = null;
            if (tile in SLOPE_DIRECTIONS) {
                newDirectionConstraint = SLOPE_DIRECTIONS[tile];
            }

            // Mark the tile as visited
            visited[newY][newX] = true;

            // Recurse with updated parameters
            dfs(newX, newY, visited, newDirectionConstraint, steps + 1);

            // Backtrack: unmark the tile as visited
            visited[newY][newX] = false;
        }
    }

    // Initialize visited matrix
    const visited = Array.from({ length: height }, () => Array(width).fill(false));

    // Mark the starting position as visited
    visited[start.y][start.x] = true;

    // Start DFS from the starting position
    dfs(start.x, start.y, visited, null, 0);

    return maxSteps;
}

/**
 * Main function to execute the program.
 */
function main() {
    try {
        const grid = readGrid();
        const { start, end } = findStartEnd(grid);
        const longestHike = findLongestHike(grid, start, end);
        console.log(longestHike);
    } catch (error) {
        console.error("Error:", error.message);
    }
}

main();
