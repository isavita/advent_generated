
const fs = require('node:fs');

const North = { x: 0, y: -1 };
const South = { x: 0, y: 1 };
const West = { x: -1, y: 0 };
const East = { x: 1, y: 0 };

const Empty = '.';
const Wall = '#';
const NorthSlopes = '^';
const SouthSlopes = 'v';
const WestSlopes = '<';
const EastSlopes = '>';

const SlopeToDir = {
    [NorthSlopes]: North,
    [SouthSlopes]: South,
    [WestSlopes]: West,
    [EastSlopes]: East,
};

function addCoords(c1, c2) {
    return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function isInBounds(grid, coord) {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}

function parseInput(input) {
    const height = input.length;
    const width = input[0].length;
    const data = new Map();

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const char = input[y][x];
            if (char !== Empty) {
                data.set(`${x},${y}`, char);
            }
        }
    }

    return { width, height, data };
}

function isValidNeighbor(grid, coord, dir) {
    if (!isInBounds(grid, coord)) {
        return false;
    }
    if (grid.data.get(`${coord.x},${coord.y}`) === Wall) {
        return false;
    }
    return true;
}

function isValidNeighborWithSlopes(grid, coord, dir) {
    if (!isInBounds(grid, coord)) {
        return false;
    }
    const char = grid.data.get(`${coord.x},${coord.y}`);
    if (char === undefined) {
        return true;
    }
    if (char === Wall) {
        return false;
    }
    return SlopeToDir[char] === dir;
}

function neighbors4(grid, coord, isValidNeighborFunc) {
    const directions = [North, South, West, East];
    const validNeighbors = [];

    for (const dir of directions) {
        const neighbor = addCoords(coord, dir);
        if (isValidNeighborFunc(grid, neighbor, dir)) {
            validNeighbors.push(neighbor);
        }
    }

    return validNeighbors;
}

function getGraph(grid, start, end, isValidNeighborFunc) {
    const vertices = new Map();
    vertices.set(`${start.x},${start.y}`, true);
    vertices.set(`${end.x},${end.y}`, true);
    const edges = new Map();

    for (let y = 0; y < grid.height; y++) {
        for (let x = 0; x < grid.width; x++) {
            const coord = { x, y };
            if (!grid.data.has(`${x},${y}`)) {
                if (neighbors4(grid, coord, isValidNeighbor).length > 2) {
                    vertices.set(`${x},${y}`, true);
                }
            }
        }
    }

    for (const vertex of vertices.keys()) {
        const [x,y] = vertex.split(",").map(Number);
        const startCoord = {x,y};
        const vertexEdges = getEdgesBFS(grid, startCoord, vertices, isValidNeighborFunc);
        edges.set(vertex, vertexEdges);
    }

    return { vertices, edges };
}

function getEdgesBFS(grid, start, vertices, isValidNeighborFunc) {
    const frontier = [start];
    const reached = new Map();
    reached.set(`${start.x},${start.y}`, true);
    const distances = new Map();
    distances.set(`${start.x},${start.y}`, 0);
    const edges = new Map();

    while (frontier.length > 0) {
        const current = frontier.shift();
        const currentKey = `${current.x},${current.y}`;

        if (vertices.has(currentKey) && (current.x !== start.x || current.y !== start.y)) {
            const edge = {
                start: start,
                end: current,
                weight: distances.get(currentKey),
            };
            edges.set(`${edge.end.x},${edge.end.y}`, edge);
            continue;
        }

        for (const next of neighbors4(grid, current, isValidNeighborFunc)) {
            const nextKey = `${next.x},${next.y}`;
            if (!reached.has(nextKey)) {
                frontier.push(next);
                reached.set(nextKey, true);
                distances.set(nextKey, distances.get(currentKey) + 1);
            }
        }
    }

    return edges;
}

function getMaxDistanceDFS(grid, graph, current, end, seen) {
    if (current.x === end.x && current.y === end.y) {
        return [true, 0];
    }

    seen.set(`${current.x},${current.y}`, true);
    let maxi = 0;
    const currentEdges = graph.edges.get(`${current.x},${current.y}`);
    if(currentEdges){
        for (const edge of currentEdges.values()) {
            const endKey = `${edge.end.x},${edge.end.y}`;
            if (!seen.has(endKey)) {
                const [isValid, dist] = getMaxDistanceDFS(grid, graph, edge.end, end, seen);
                if (isValid) {
                    maxi = Math.max(maxi, dist + edge.weight);
                }
            }
        }
    }
    seen.delete(`${current.x},${current.y}`);

    if (maxi === 0) {
        return [false, 0];
    }
    return [true, maxi];
}

function solve(input) {
    const grid = parseInput(input);
    const start = { x: 1, y: 0 };
    const end = { x: grid.width - 2, y: grid.height - 1 };

    const graph = getGraph(grid, start, end, isValidNeighbor);
    const [_, maxDist] = getMaxDistanceDFS(grid, graph, start, end, new Map());
    return maxDist;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
console.log(solve(input));
