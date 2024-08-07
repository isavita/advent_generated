type Coord = {
  X: number;
  Y: number;
};

const add = (c1: Coord, c2: Coord): Coord => ({ X: c1.X + c2.X, Y: c1.Y + c2.Y });
const multiplyByScalar = (c: Coord, s: number): Coord => ({ X: c.X * s, Y: c.Y * s });

const North: Coord = { X: 0, Y: -1 };
const West: Coord = { X: -1, Y: 0 };
const South: Coord = { X: 0, Y: 1 };
const East: Coord = { X: 1, Y: 0 };

const abs = (x: number): number => (x < 0 ? -x : x);

const parseInput = (input: string[]): Coord[] => {
  const Up = '3';
  const Left = '2';
  const Down = '1';
  const Right = '0';

  let current: Coord = { X: 0, Y: 0 };
  const vertices: Coord[] = [current];

  for (const line of input) {
    const parts = line.split(' ');
    const color = parts[2];
    const dirInput = color[7];
    const lengthStr = color.slice(2, 7);
    const length = parseInt(lengthStr, 16);

    let dir: Coord;
    switch (dirInput) {
      case Up:
        dir = North;
        break;
      case Left:
        dir = West;
        break;
      case Down:
        dir = South;
        break;
      case Right:
        dir = East;
        break;
      default:
        throw new Error(`Invalid direction input: ${dirInput}`);
    }

    current = add(current, multiplyByScalar(dir, length));
    vertices.push(current);
  }

  return vertices;
};

const shoelace = (vertices: Coord[]): number => {
  const n = vertices.length;
  let area = 0;

  for (let i = 0; i < n; i++) {
    const next = (i + 1) % n;
    area += vertices[i].X * vertices[next].Y;
    area -= vertices[i].Y * vertices[next].X;
  }

  return abs(area) / 2;
};

const perimeter = (vertices: Coord[]): number => {
  const n = vertices.length;
  let perim = 0;

  for (let i = 0; i < n; i++) {
    const next = (i + 1) % n;
    perim += abs(vertices[i].X - vertices[next].X) + abs(vertices[i].Y - vertices[next].Y);
  }

  return perim;
};

const calculatePolygonArea = (vertices: Coord[]): number => shoelace(vertices) + perimeter(vertices) / 2 + 1;

const solve = (input: string[]): number => {
  const vertices = parseInput(input);
  return calculatePolygonArea(vertices);
};

const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
console.log(solve(input));