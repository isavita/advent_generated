import * as fs from 'fs';
import * as path from 'path';

type Coord = { x: number; y: number };

const addCoord = (c1: Coord, c2: Coord): Coord => ({ x: c1.x + c2.x, y: c1.y + c2.y });

type Grid = {
  width: number;
  height: number;
  data: Map<string, string>;
};

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

const SlopeToDir = new Map<string, Coord>([
  [NorthSlopes, North],
  [SouthSlopes, South],
  [WestSlopes, West],
  [EastSlopes, East],
]);

type Edge = {
  start: Coord;
  end: Coord;
  weight: number;
};

type Graph = {
  vertices: Set<string>;
  edges: Map<string, Map<string, Edge>>;
};

const isInBounds = (grid: Grid, coord: Coord): boolean =>
  coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;

const parseInput = (input: string[]): Grid => {
  const grid: Grid = {
    width: input[0].length,
    height: input.length,
    data: new Map(),
  };

  input.forEach((line, y) => {
    [...line].forEach((char, x) => {
      if (char !== Empty) {
        grid.data.set(`${x},${y}`, char);
      }
    });
  });

  return grid;
};

const isValidNeighbor = (grid: Grid, coord: Coord): boolean =>
  isInBounds(grid, coord) && grid.data.get(`${coord.x},${coord.y}`) !== Wall;

const isValidNeighborWithSlopes = (grid: Grid, coord: Coord, dir: Coord): boolean =>
  isInBounds(grid, coord) &&
  (grid.data.get(`${coord.x},${coord.y}`) === undefined ||
    (grid.data.get(`${coord.x},${coord.y}`) !== Wall &&
      SlopeToDir.get(grid.data.get(`${coord.x},${coord.y}`)!) === dir));

const neighbors4 = (grid: Grid, coord: Coord, isValidNeighborFunc: (grid: Grid, coord: Coord, dir: Coord) => boolean): Coord[] => {
  const directions = [North, South, West, East];
  const validNeighbors: Coord[] = [];

  directions.forEach((dir) => {
    const neighbor = addCoord(coord, dir);
    if (isValidNeighborFunc(grid, neighbor, dir)) {
      validNeighbors.push(neighbor);
    }
  });

  return validNeighbors;
};

const getGraph = (grid: Grid, start: Coord, end: Coord, isValidNeighborFunc: (grid: Grid, coord: Coord, dir: Coord) => boolean): Graph => {
  const graph: Graph = {
    vertices: new Set([`${start.x},${start.y}`, `${end.x},${end.y}`]),
    edges: new Map(),
  };

  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      const coord: Coord = { x, y };
      if (!grid.data.has(`${coord.x},${coord.y}`) && neighbors4(grid, coord, isValidNeighbor).length > 2) {
        graph.vertices.add(`${coord.x},${coord.y}`);
      }
    }
  }

  graph.vertices.forEach((vertex) => {
    const [x, y] = vertex.split(',').map(Number);
    const edges = getEdgesBFS(grid, { x, y }, graph.vertices, isValidNeighborFunc);
    graph.edges.set(vertex, edges);
  });

  return graph;
};

const getEdgesBFS = (grid: Grid, start: Coord, vertices: Set<string>, isValidNeighborFunc: (grid: Grid, coord: Coord, dir: Coord) => boolean): Map<string, Edge> => {
  const frontier: Coord[] = [start];
  const reached = new Set([`${start.x},${start.y}`]);
  const distances = new Map([[`${start.x},${start.y}`, 0]]);
  const edges = new Map<string, Edge>();

  while (frontier.length > 0) {
    const current = frontier.shift()!;

    if (vertices.has(`${current.x},${current.y}`) && !(current.x === start.x && current.y === start.y)) {
      const edge: Edge = {
        start,
        end: current,
        weight: distances.get(`${current.x},${current.y}`)!,
      };
      edges.set(`${edge.end.x},${edge.end.y}`, edge);
      continue;
    }

    neighbors4(grid, current, isValidNeighborFunc).forEach((next) => {
      const key = `${next.x},${next.y}`;
      if (!reached.has(key)) {
        frontier.push(next);
        reached.add(key);
        distances.set(key, distances.get(`${current.x},${current.y}`)! + 1);
      }
    });
  }

  return edges;
};

const getMaxDistanceDFS = (grid: Grid, graph: Graph, current: Coord, end: Coord, seen: Set<string>): [boolean, number] => {
  if (current.x === end.x && current.y === end.y) {
    return [true, 0];
  }

  let maxi = 0;
  seen.add(`${current.x},${current.y}`);
  const currentEdges = graph.edges.get(`${current.x},${current.y}`)!;
  currentEdges.forEach((edge) => {
    if (!seen.has(`${edge.end.x},${edge.end.y}`)) {
      const [isValid, dist] = getMaxDistanceDFS(grid, graph, edge.end, end, seen);
      if (isValid) {
        maxi = Math.max(maxi, dist + edge.weight);
      }
    }
  });
  seen.delete(`${current.x},${current.y}`);

  if (maxi === 0) {
    return [false, 0];
  }
  return [true, maxi];
};

const solve = (input: string[]): number => {
  const grid = parseInput(input);

  const start: Coord = { x: 1, y: 0 };
  const end: Coord = { x: grid.width - 2, y: grid.height - 1 };

  const graph = getGraph(grid, start, end, isValidNeighbor);

  const [_, maxDist] = getMaxDistanceDFS(grid, graph, start, end, new Set());
  return maxDist;
};

const readFile = async (fileName: string): Promise<string[]> => {
  const filePath = path.resolve(__dirname, fileName);
  const data = fs.readFileSync(filePath, 'utf-8');
  return data.trim().split('\n');
};

readFile('input.txt').then((input) => {
  console.log(solve(input));
});