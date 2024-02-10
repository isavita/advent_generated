const fs = require('fs');

function main() {
  const input = fs.readFileSync('./input.txt', 'utf8').trim();
  const n = cleaningRobot(input);
  console.log(n);
}

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

  return dfs(graph, 0, {0: true}, false);
}

function bfsGetEdgeWeights(grid, start) {
  const poiToDistance = {
    [grid[start[0]][start[1]]]: 0,
  };

  const queue = [{row: start[0], col: start[1], distance: 0}];
  const visited = {};
  while (queue.length) {
    const front = queue.shift();

    if (visited[`${front.row},${front.col}`]) {
      continue;
    }
    visited[`${front.row},${front.col}`] = true;

    if (/[0-9]/.test(grid[front.row][front.col])) {
      poiToDistance[grid[front.row][front.col]] = front.distance;
    }
    for (const [dx, dy] of [[0, -1], [0, 1], [1, 0], [-1, 0]]) {
      const nextRow = front.row + dx;
      const nextCol = front.col + dy;

      if (grid[nextRow][nextCol] !== '#') {
        queue.push({row: nextRow, col: nextCol, distance: front.distance + 1});
      }
    }
  }

  const distances = Array.from({length: Object.keys(poiToDistance).length}, () => 0);
  for (const [numStr, dist] of Object.entries(poiToDistance)) {
    const n = parseInt(numStr);
    distances[n] = dist;
  }
  return distances;
}

function dfs(graph, entryIndex, visited, returnToZero) {
  if (graph.length === Object.keys(visited).length) {
    if (returnToZero) {
      return graph[entryIndex][0];
    }
    return 0;
  }

  let minDistance = Number.MAX_SAFE_INTEGER;
  for (const [i, val] of graph[entryIndex].entries()) {
    if (!visited[i]) {
      visited[i] = true;

      const dist = val + dfs(graph, i, visited, returnToZero);
      minDistance = Math.min(minDistance, dist);

      delete visited[i];
    }
  }

  return minDistance;
}

main();