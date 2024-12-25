
type Coord = { x: number; y: number };

const Undefined: Coord = { x: 0, y: 0 };
const Top: Coord = { x: 0, y: -1 };
const Right: Coord = { x: 1, y: 0 };
const Bottom: Coord = { x: 0, y: 1 };
const Left: Coord = { x: -1, y: 0 };

const Empty = ".";
const Start = "S";
const Vertical = "|";
const Horizontal = "-";
const TopLeftCorner = "J";
const TopRightCorner = "L";
const BottomLeftCorner = "7";
const BottomRightCorner = "F";

const TileToPipe = new Map<string, Coord[]>([
  [Vertical, [Top, Bottom]],
  [Horizontal, [Left, Right]],
  [TopLeftCorner, [Top, Left]],
  [TopRightCorner, [Top, Right]],
  [BottomLeftCorner, [Bottom, Left]],
  [BottomRightCorner, [Bottom, Right]],
]);

function add(c1: Coord, c2: Coord): Coord {
  return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function opposite(c: Coord): Coord {
  return { x: -c.x, y: -c.y };
}

function getPipeFromTile(tile: string): Coord[] {
  return TileToPipe.get(tile) || [];
}

function buildGrid(input: string[]): { grid: string[][]; start: Coord } {
  const grid: string[][] = [];
  let start: Coord = Undefined;
  for (let y = 0; y < input.length; y++) {
    grid[y] = [];
    for (let x = 0; x < input[y].length; x++) {
      const char = input[y][x];
      grid[y][x] = char;
      if (char === Start) {
        start = { x, y };
      }
    }
  }
  return { grid, start };
}

function getPipeFromNeighbors(grid: string[][], start: Coord): Coord[] {
  const pipe: Coord[] = [];
  const possibleNeighbors = new Map<Coord, Coord>([
    [Top, add(start, Top)],
    [Right, add(start, Right)],
    [Bottom, add(start, Bottom)],
    [Left, add(start, Left)],
  ]);

  for (const [dir, neighborCoord] of possibleNeighbors) {
    if (
      neighborCoord.y >= 0 &&
      neighborCoord.y < grid.length &&
      neighborCoord.x >= 0 &&
      neighborCoord.x < grid[neighborCoord.y].length
    ) {
      const neighborPipe = getPipeFromTile(
        grid[neighborCoord.y][neighborCoord.x]
      );
      if (neighborPipe.some((d) => d.x === opposite(dir).x && d.y === opposite(dir).y)) {
        pipe.push(dir);
      }
    }
  }
  return pipe;
}

function pathFinding(grid: string[][], start: Coord): Coord[] {
  const path: Coord[] = [start];
  const startPipe = getPipeFromNeighbors(grid, start);

  let previousDir = startPipe[0];
  let current = add(start, previousDir);

  while (current.x !== start.x || current.y !== start.y) {
    path.push(current);
    const currentPipe = getPipeFromTile(grid[current.y][current.x]);
    for (const dir of currentPipe) {
      if (dir.x !== opposite(previousDir).x || dir.y !== opposite(previousDir).y) {
        previousDir = dir;
        current = add(current, dir);
        break;
      }
    }
  }
  return path;
}

function solve(input: string[]): number {
  const { grid, start } = buildGrid(input);
  const path = pathFinding(grid, start);
  return path.length / 2;
}

const input = require("fs").readFileSync("input.txt", "utf-8").trim().split("\n");
console.log(solve(input));
