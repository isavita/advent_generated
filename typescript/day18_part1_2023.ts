import * as fs from 'fs';

type Coord = { x: number; y: number };

const North: Coord = { x: 0, y: -1 };
const West: Coord = { x: -1, y: 0 };
const South: Coord = { x: 0, y: 1 };
const East: Coord = { x: 1, y: 0 };

const Abs = (x: number): number => (x < 0 ? -x : x);

const parseInput = (input: string[]): Coord[] => {
    const vertices: Coord[] = [{ x: 0, y: 0 }];
    let current: Coord = vertices[0];

    for (const line of input) {
        const [dirInput, lengthStr] = line.split(" ");
        const length = parseInt(lengthStr, 10);
        let dir: Coord;

        switch (dirInput) {
            case 'U': dir = North; break;
            case 'L': dir = West; break;
            case 'D': dir = South; break;
            case 'R': dir = East; break;
            default: throw new Error(`Invalid direction: ${dirInput}`);
        }

        current = { x: current.x + dir.x * length, y: current.y + dir.y * length };
        vertices.push(current);
    }

    return vertices;
};

const shoelace = (vertices: Coord[]): number => {
    const n = vertices.length;
    let area = 0;

    for (let i = 0; i < n; i++) {
        const next = (i + 1) % n;
        area += vertices[i].x * vertices[next].y - vertices[i].y * vertices[next].x;
    }

    return Abs(area) / 2;
};

const perimeter = (vertices: Coord[]): number => {
    const n = vertices.length;
    let perim = 0;

    for (let i = 0; i < n; i++) {
        const next = (i + 1) % n;
        perim += Abs(vertices[i].x - vertices[next].x) + Abs(vertices[i].y - vertices[next].y);
    }

    return perim;
};

const calculatePolygonArea = (vertices: Coord[]): number => {
    return shoelace(vertices) + Math.floor(perimeter(vertices) / 2) + 1;
};

const solve = (input: string[]): number => {
    const vertices = parseInput(input);
    return calculatePolygonArea(vertices);
};

const readFile = (fileName: string): string[] => {
    return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
};

const main = () => {
    const input = readFile("input.txt");
    console.log(solve(input));
};

main();