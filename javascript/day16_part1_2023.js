// floorWillBeLava.js

const fs = require('fs');
const path = require('path');

// Directions with their corresponding (dx, dy)
const DIRECTIONS = {
    'up': { dx: 0, dy: -1 },
    'down': { dx: 0, dy: 1 },
    'left': { dx: -1, dy: 0 },
    'right': { dx: 1, dy: 0 },
};

// Mirror reflection mappings
const MIRROR_REFLECTION = {
    '/': {
        'up': 'right',
        'down': 'left',
        'left': 'down',
        'right': 'up',
    },
    '\\': {
        'up': 'left',
        'down': 'right',
        'left': 'up',
        'right': 'down',
    },
};

// Read input.txt
const inputPath = path.join(__dirname, 'input.txt');
const input = fs.readFileSync(inputPath, 'utf-8');
const lines = input.trim().split('\n');

// Parse grid
const grid = lines.map(line => line.split(''));
const height = grid.length;
const width = grid[0].length;

// Set to keep track of energized tiles as 'x,y' strings
const energizedTiles = new Set();

// Queue for beams to process
const beamQueue = [];

// Set to keep track of visited beam states to avoid infinite loops
const visited = new Set();

// Initialize the beam: entering from left to (0,0) moving right
beamQueue.push({ x: -1, y: 0, direction: 'right' });

// Process beams until the queue is empty
while (beamQueue.length > 0) {
    const beam = beamQueue.shift();
    const { x, y, direction } = beam;

    // Calculate next position
    const { dx, dy } = DIRECTIONS[direction];
    const nextX = x + dx;
    const nextY = y + dy;

    // Check if the beam is outside the grid
    if (nextX < 0 || nextX >= width || nextY < 0 || nextY >= height) {
        // Beam exits the grid; no further processing
        continue;
    }

    // Unique state identifier
    const stateKey = `${nextX},${nextY},${direction}`;
    if (visited.has(stateKey)) {
        // Already processed this state; skip to prevent infinite loops
        continue;
    }
    visited.add(stateKey);

    // Mark the current tile as energized
    energizedTiles.add(`${nextX},${nextY}`);

    // Get the current tile type
    const tile = grid[nextY][nextX];

    if (tile === '.') {
        // Empty space; continue in the same direction
        beamQueue.push({ x: nextX, y: nextY, direction });
    } else if (tile === '/' || tile === '\\') {
        // Mirror; reflect the beam
        const newDirection = MIRROR_REFLECTION[tile][direction];
        if (newDirection) {
            beamQueue.push({ x: nextX, y: nextY, direction: newDirection });
        }
        // If mirror doesn't define a reflection for the current direction, the beam stops
    } else if (tile === '|' || tile === '-') {
        // Splitter; determine if beam hits flat side or pointy end
        let splitDirections = [];

        if (tile === '|') {
            if (direction === 'left' || direction === 'right') {
                // Hitting flat side; split into 'up' and 'down'
                splitDirections = ['up', 'down'];
            } else {
                // Hitting pointy end; continue in the same direction
                splitDirections = [direction];
            }
        } else if (tile === '-') {
            if (direction === 'up' || direction === 'down') {
                // Hitting flat side; split into 'left' and 'right'
                splitDirections = ['left', 'right'];
            } else {
                // Hitting pointy end; continue in the same direction
                splitDirections = [direction];
            }
        }

        // Enqueue new beams based on split directions
        for (const newDir of splitDirections) {
            beamQueue.push({ x: nextX, y: nextY, direction: newDir });
        }
    }
    // Any other tile types are treated as obstacles; the beam stops
}

// Output the number of energized tiles
console.log(energizedTiles.size);
