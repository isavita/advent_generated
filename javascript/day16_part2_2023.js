// floorWillBeLavaPartTwo.js

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

// Function to identify all possible starting configurations (just outside the grid)
function getStartingConfigurations() {
    const configurations = [];

    // Top Edge: Start above the grid, moving down
    for (let x = 0; x < width; x++) {
        configurations.push({ x: x, y: -1, direction: 'down' });
    }

    // Bottom Edge: Start below the grid, moving up
    for (let x = 0; x < width; x++) {
        configurations.push({ x: x, y: height, direction: 'up' });
    }

    // Left Edge: Start to the left of the grid, moving right
    for (let y = 0; y < height; y++) {
        configurations.push({ x: -1, y: y, direction: 'right' });
    }

    // Right Edge: Start to the right of the grid, moving left
    for (let y = 0; y < height; y++) {
        configurations.push({ x: width, y: y, direction: 'left' });
    }

    // Handle Corner Tiles with two possible directions each
    const corners = [
        { x: 0, y: 0, directions: ['right', 'down'] }, // Top-Left
        { x: width - 1, y: 0, directions: ['left', 'down'] }, // Top-Right
        { x: 0, y: height - 1, directions: ['right', 'up'] }, // Bottom-Left
        { x: width - 1, y: height - 1, directions: ['left', 'up'] }, // Bottom-Right
    ];

    corners.forEach(corner => {
        corner.directions.forEach(dir => {
            // Adjust starting positions for corners
            if (dir === 'right') {
                configurations.push({ x: corner.x - 1, y: corner.y, direction: 'right' });
            } else if (dir === 'left') {
                configurations.push({ x: corner.x + 1, y: corner.y, direction: 'left' });
            } else if (dir === 'down') {
                configurations.push({ x: corner.x, y: corner.y - 1, direction: 'down' });
            } else if (dir === 'up') {
                configurations.push({ x: corner.x, y: corner.y + 1, direction: 'up' });
            }
        });
    });

    return configurations;
}

// BFS Function to simulate beam path
function bfs(startPos) {
    const { x, y, direction } = startPos;

    // Set to keep track of energized tiles as 'x,y' strings
    const energizedTiles = new Set();

    // Queue for beams to process
    const beamQueue = [];
    beamQueue.push({ x: x, y: y, direction: direction });

    // Set to keep track of visited beam states to avoid infinite loops
    const visited = new Set();

    while (beamQueue.length > 0) {
        const beam = beamQueue.shift();
        const { x: beamX, y: beamY, direction: beamDir } = beam;

        // Calculate next position
        const { dx, dy } = DIRECTIONS[beamDir];
        const nextX = beamX + dx;
        const nextY = beamY + dy;

        // Check if the beam is outside the grid
        if (nextX < 0 || nextX >= width || nextY < 0 || nextY >= height) {
            // Beam exits the grid; no further processing
            continue;
        }

        // Unique state identifier
        const stateKey = `${nextX},${nextY},${beamDir}`;
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
            beamQueue.push({ x: nextX, y: nextY, direction: beamDir });
        } else if (tile === '/' || tile === '\\') {
            // Mirror; reflect the beam
            const newDirection = MIRROR_REFLECTION[tile][beamDir];
            if (newDirection) {
                beamQueue.push({ x: nextX, y: nextY, direction: newDirection });
            }
            // If mirror doesn't define a reflection for the current direction, the beam stops
        } else if (tile === '|' || tile === '-') {
            // Splitter; determine if beam hits flat side or pointy end
            let splitDirections = [];

            if (tile === '|') {
                if (beamDir === 'left' || beamDir === 'right') {
                    // Hitting flat side; split into 'up' and 'down'
                    splitDirections = ['up', 'down'];
                } else {
                    // Hitting pointy end; continue in the same direction
                    splitDirections = [beamDir];
                }
            } else if (tile === '-') {
                if (beamDir === 'up' || beamDir === 'down') {
                    // Hitting flat side; split into 'left' and 'right'
                    splitDirections = ['left', 'right'];
                } else {
                    // Hitting pointy end; continue in the same direction
                    splitDirections = [beamDir];
                }
            }

            // Enqueue new beams based on split directions
            for (const newDir of splitDirections) {
                beamQueue.push({ x: nextX, y: nextY, direction: newDir });
            }
        }
        // Any other tile types are treated as obstacles; the beam stops
    }

    return energizedTiles;
}

// Main Execution

// Get all possible starting configurations
const startingConfigs = getStartingConfigurations();

// Initialize variables to track the maximum energized tiles
let maxEnergized = 0;
// Optional: To track which configuration achieves the max
let bestConfig = null;

// Iterate through each starting configuration
startingConfigs.forEach(config => {
    const energized = bfs(config);
    const count = energized.size;

    if (count > maxEnergized) {
        maxEnergized = count;
        bestConfig = config;
    }
});

// Output the maximum number of energized tiles
console.log(maxEnergized);
