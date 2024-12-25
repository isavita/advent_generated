
type Coord = { x: number; y: number };

const North: Coord = { x: 0, y: -1 };
const South: Coord = { x: 0, y: 1 };
const West: Coord = { x: -1, y: 0 };
const East: Coord = { x: 1, y: 0 };

const Empty = '.';
const Wall = '#';
const NorthSlopes = '^';
const SouthSlopes = 'v';
const WestSlopes = '<';
const EastSlopes = '>';

const SlopeToDir: Record<string, Coord> = {
  [NorthSlopes]: North,
  [SouthSlopes]: South,
  [WestSlopes]: West,
  [EastSlopes]: East,
};

type Edge = { start: Coord; end: Coord; weight: number };

type Graph = {
  vertices: Set<string>;
  edges: Record<string, Edge[]>;
};

function coordToString(c: Coord): string {
  return `${c.x},${c.y}`;
}

function stringToCoord(s: string): Coord {
  const [x, y] = s.split(',').map(Number);
  return { x, y };
}

function addCoords(c1: Coord, c2: Coord): Coord {
  return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function isInBounds(grid: string[], coord: Coord): boolean {
  return 0 <= coord.x && coord.x < grid[0].length && 0 <= coord.y && coord.y < grid.length;
}

function isValidNeighbor(grid: string[], coord: Coord, dir: Coord): boolean {
  if (!isInBounds(grid, coord)) {
    return false;
  }
  return grid[coord.y][coord.x] !== Wall;
}

function isValidNeighborWithSlopes(grid: string[], coord: Coord, dir: Coord): boolean {
  if (!isInBounds(grid, coord)) {
    return false;
  }
  const cell = grid[coord.y][coord.x];
  if (cell === Empty) {
    return true;
  }
  if (cell === Wall) {
    return false;
  }
  return SlopeToDir[cell].x === dir.x && SlopeToDir[cell].y === dir.y;
}

function neighbors4(
  grid: string[],
  coord: Coord,
  isValidNeighborFunc: (grid: string[], coord: Coord, dir: Coord) => boolean
): Coord[] {
  const directions = [North, South, West, East];
  const validNeighbors: Coord[] = [];

  for (const dir of directions) {
    const neighbor = addCoords(coord, dir);
    if (isValidNeighborFunc(grid, neighbor, dir)) {
      validNeighbors.push(neighbor);
    }
  }

  return validNeighbors;
}

function getGraph(
  grid: string[],
  start: Coord,
  end: Coord,
  isValidNeighborFunc: (grid: string[], coord: Coord, dir: Coord) => boolean
): Graph {
  const vertices = new Set<string>([coordToString(start), coordToString(end)]);
  const edges: Record<string, Edge[]> = {};

  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[0].length; x++) {
      const coord = { x, y };
      if (grid[y][x] !== Empty) continue;
      if (neighbors4(grid, coord, isValidNeighbor).length > 2) {
        vertices.add(coordToString(coord));
      }
    }
  }

  for (const vertex of vertices) {
    edges[vertex] = getEdgesBFS(grid, stringToCoord(vertex), vertices, isValidNeighborFunc);
  }

  return { vertices, edges };
}

function getEdgesBFS(
  grid: string[],
  start: Coord,
  vertices: Set<string>,
  isValidNeighborFunc: (grid: string[], coord: Coord, dir: Coord) => boolean
): Edge[] {
  const frontier = [start];
  const reached = new Set<string>([coordToString(start)]);
  const distances = new Map<string, number>([[coordToString(start), 0]]);
  const result: Edge[] = [];

  while (frontier.length > 0) {
    const current = frontier.shift()!;
    const currentStr = coordToString(current);

    if (vertices.has(currentStr) && currentStr !== coordToString(start)) {
      result.push({
        start,
        end: current,
        weight: distances.get(currentStr)!,
      });
      continue;
    }

    for (const next of neighbors4(grid, current, isValidNeighborFunc)) {
      const nextStr = coordToString(next);
      if (!reached.has(nextStr)) {
        frontier.push(next);
        reached.add(nextStr);
        distances.set(nextStr, distances.get(currentStr)! + 1);
      }
    }
  }

  return result;
}

function getMaxDistanceDFS(
  graph: Graph,
  current: Coord,
  end: Coord,
  seen: Set<string>
): number {
  const currentStr = coordToString(current);
  if (currentStr === coordToString(end)) {
    return 0;
  }

  let maxDist = -Infinity;
  seen.add(currentStr);

  for (const edge of graph.edges[currentStr]) {
    const endStr = coordToString(edge.end);
    if (!seen.has(endStr)) {
      const dist = getMaxDistanceDFS(graph, edge.end, end, seen);
      if (dist !== -Infinity) {
        maxDist = Math.max(maxDist, dist + edge.weight);
      }
    }
  }

  seen.delete(currentStr);
  return maxDist;
}

function solve(input: string[]): number {
  const start: Coord = { x: 1, y: 0 };
  const end: Coord = { x: input[0].length - 2, y: input.length - 1 };

  const graph = getGraph(input, start, end, isValidNeighborWithSlopes);

  return getMaxDistanceDFS(graph, start, end, new Set<string>());
}

const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
console.log(solve(input));
