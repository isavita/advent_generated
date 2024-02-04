const fs = require('fs');

function cleaningRobot(input) {
    const grid = input.split('\n').map(row => row.split(''));

    let graph = [];
    grid.forEach((row, r) => {
        row.forEach((cell, c) => {
            if (/[0-9]/.test(cell)) {
                const poi = cell;
                const distancesFromPOI = bfsGetEdgeWeights(grid, [r, c]);

                if (!graph.length) {
                    for (let i = 0; i < distancesFromPOI.length; i++) {
                        graph.push(new Array(distancesFromPOI.length).fill(0));
                    }
                }
                const index = parseInt(poi);
                graph[index] = distancesFromPOI;
            }
        });
    });

    return dfs(graph, 0, {0: true}, true);
}

function bfsGetEdgeWeights(grid, start) {
    const poiToDistance = {
        [grid[start[0]][start[1]]]: 0
    };

    const queue = [{
        row: start[0],
        col: start[1],
        distance: 0
    }];
    const visited = {};
    while (queue.length > 0) {
        const front = queue.shift();

        if (visited[`${front.row},${front.col}`]) {
            continue;
        }
        visited[`${front.row},${front.col}`] = true;

        if (/[0-9]/.test(grid[front.row][front.col])) {
            poiToDistance[grid[front.row][front.col]] = front.distance;
        }
        for (const [dx, dy] of dirs) {
            const nextRow = front.row + dx;
            const nextCol = front.col + dy;

            if (grid[nextRow][nextCol] !== "#") {
                queue.push({
                    row: nextRow,
                    col: nextCol,
                    distance: front.distance + 1
                });
            }
        }
    }

    const distances = new Array(Object.keys(poiToDistance).length).fill(0);
    for (const numStr in poiToDistance) {
        const n = parseInt(numStr);
        distances[n] = poiToDistance[numStr];
    }
    return distances;
}

const dirs = [
    [0, -1],
    [0, 1],
    [1, 0],
    [-1, 0]
];

function dfs(graph, entryIndex, visited, returnToZero) {
    if (graph.length === Object.keys(visited).length) {
        if (returnToZero) {
            return graph[entryIndex][0];
        }
        return 0;
    }

    let minDistance = Number.MAX_SAFE_INTEGER;
    for (let i = 0; i < graph[entryIndex].length; i++) {
        if (!visited[i]) {
            visited[i] = true;

            const dist = graph[entryIndex][i] + dfs(graph, i, visited, returnToZero);
            minDistance = Math.min(minDistance, dist);

            delete visited[i];
        }
    }

    return minDistance;
}

const input = fs.readFileSync('input.txt', 'utf8');
const result = cleaningRobot(input);
console.log(result);