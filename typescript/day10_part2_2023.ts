
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

type Pipe = Set<string>;

const VerticalPipe: Pipe = new Set(["Top", "Bottom"]);
const HorizontalPipe: Pipe = new Set(["Left", "Right"]);
const TopLeftCornerPipe: Pipe = new Set(["Top", "Left"]);
const TopRightCornerPipe: Pipe = new Set(["Top", "Right"]);
const BottomLeftCornerPipe: Pipe = new Set(["Bottom", "Left"]);
const BottomRightCornerPipe: Pipe = new Set(["Bottom", "Right"]);

const TileToPipe: Record<string, Pipe> = {
  [Vertical]: VerticalPipe,
  [Horizontal]: HorizontalPipe,
  [TopLeftCorner]: TopLeftCornerPipe,
  [TopRightCorner]: TopRightCornerPipe,
  [BottomLeftCorner]: BottomLeftCornerPipe,
  [BottomRightCorner]: BottomRightCornerPipe,
};

function getPipeFromTile(tile: string): Pipe {
  return TileToPipe[tile] || new Set();
}

function getTileFromPipe(pipe: Pipe): string {
  for (const tile in TileToPipe) {
    if (isEqualPipe(pipe, TileToPipe[tile])) {
      return tile;
    }
  }
  return Empty;
}

function isEqualPipe(pipe1: Pipe, pipe2: Pipe): boolean {
  if (pipe1.size !== pipe2.size) {
    return false;
  }
  for (const dir of pipe1) {
    if (!pipe2.has(dir)) {
      return false;
    }
  }
  return true;
}

type Grid = {
  width: number;
  height: number;
  data: Map<string, string>;
};

function buildGrid(input: string[]): Grid {
  const grid: Grid = {
    width: input[0].length,
    height: input.length,
    data: new Map(),
  };
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      if (input[y][x] !== Empty) {
        grid.data.set(`${x},${y}`, input[y][x]);
      }
    }
  }
  return grid;
}

function findStart(grid: Grid): Coord {
  for (const [coordStr, value] of grid.data) {
    if (value === Start) {
      const [x, y] = coordStr.split(",").map(Number);
      return { x, y };
    }
  }
  return { x: 0, y: 0 };
}

function addCoords(c1: Coord, c2: Coord): Coord {
  return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function oppositeDir(dir: string): string {
    if (dir === "Top") return "Bottom";
    if (dir === "Bottom") return "Top";
    if (dir === "Left") return "Right";
    if (dir === "Right") return "Left";
    return "";
}

function getPipeFromNeighbors(c: Coord, grid: Grid): Pipe {
  const pipe: Pipe = new Set();
  const possibleNeighbors: Record<string, Coord> = {
    Top: addCoords(c, Top),
    Right: addCoords(c, Right),
    Bottom: addCoords(c, Bottom),
    Left: addCoords(c, Left),
  };
  for (const dir in possibleNeighbors) {
    const neighborCoord = possibleNeighbors[dir];
    const neighborPipe = getPipeFromTile(
      grid.data.get(`${neighborCoord.x},${neighborCoord.y}`) || ""
    );
    if (neighborPipe.has(oppositeDir(dir))) {
      pipe.add(dir);
    }
  }
  return pipe;
}

function pathFinding(start: Coord, grid: Grid): Coord[] {
  const path: Coord[] = [start];
  const startPipe = getPipeFromNeighbors(start, grid);
  let previousDir: string = "";
  let current: Coord = { x: 0, y: 0 };
  for (const dir of startPipe) {
    previousDir = dir;
    current = addCoords(start, {
      x: dir === "Left" ? -1 : dir === "Right" ? 1 : 0,
      y: dir === "Top" ? -1 : dir === "Bottom" ? 1 : 0,
    });
    break;
  }
  while (current.x !== start.x || current.y !== start.y) {
    path.push(current);
    const currentPipe = getPipeFromTile(
      grid.data.get(`${current.x},${current.y}`) || ""
    );
    for (const dir of currentPipe) {
      if (dir !== oppositeDir(previousDir)) {
        previousDir = dir;
        current = addCoords(current, {
          x: dir === "Left" ? -1 : dir === "Right" ? 1 : 0,
          y: dir === "Top" ? -1 : dir === "Bottom" ? 1 : 0,
        });
        break;
      }
    }
  }
  return path;
}

function getPathGrid(grid: Grid, path: Coord[]): Grid {
  const newGrid: Grid = {
    width: grid.width,
    height: grid.height,
    data: new Map(),
  };
  for (const coord of path) {
    newGrid.data.set(`${coord.x},${coord.y}`, grid.data.get(`${coord.x},${coord.y}`)!);
  }
  const start = path[0];
  newGrid.data.set(`${start.x},${start.y}`, getTileFromPipe(getPipeFromNeighbors(start, grid)));
  return newGrid;
}

function isInside(c: Coord, grid: Grid): boolean {
  if (grid.data.has(`${c.x},${c.y}`)) {
    return false;
  }
  let numPipeOnLeft = 0;
  let startPipe = "";
  for (let x = 0; x < c.x; x++) {
    const v = grid.data.get(`${x},${c.y}`);
    switch (v) {
      case Vertical:
        numPipeOnLeft++;
        break;
      case TopRightCorner:
        startPipe = TopRightCorner;
        break;
      case BottomRightCorner:
        startPipe = BottomRightCorner;
        break;
      case TopLeftCorner:
        if (startPipe === BottomRightCorner) {
          startPipe = "";
          numPipeOnLeft++;
        } else if (startPipe === TopRightCorner) {
          startPipe = "";
        }
        break;
      case BottomLeftCorner:
        if (startPipe === TopRightCorner) {
          startPipe = "";
          numPipeOnLeft++;
        } else if (startPipe === BottomRightCorner) {
          startPipe = "";
        }
        break;
    }
  }
  return numPipeOnLeft % 2 === 1;
}

function solve(input: string[]): number {
  const grid = buildGrid(input);
  const start = findStart(grid);
  const path = pathFinding(start, grid);
  const pathGrid = getPathGrid(grid, path);
  let cnt = 0;
  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      if (isInside({ x, y }, pathGrid)) {
        cnt++;
      }
    }
  }
  return cnt;
}

const input = require("fs").readFileSync("input.txt", "utf-8").trim().split("\n");
console.log(solve(input));
