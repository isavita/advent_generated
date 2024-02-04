const fs = require('fs');

class Coord {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    add(c) {
        return new Coord(this.x + c.x, this.y + c.y);
    }

    multiplyByScalar(s) {
        return new Coord(this.x * s, this.y * s);
    }
}

const North = new Coord(0, -1);
const West = new Coord(-1, 0);
const South = new Coord(0, 1);
const East = new Coord(1, 0);

function abs(x) {
    return x < 0 ? -x : x;
}

function parseInput(input) {
    const Up = 'U';
    const Left = 'L';
    const Down = 'D';
    const Right = 'R';

    let current = new Coord(0, 0);
    let vertices = [current];

    for (let line of input) {
        const parts = line.split(" ");
        const dirInput = parts[0][0];
        const lengthStr = parts[1];
        let length = 0;
        for (let i = 0; i < lengthStr.length; i++) {
            length = length * 10 + parseInt(lengthStr[i]);
        }

        let dir;
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
        }

        current = current.add(dir.multiplyByScalar(length));
        vertices.push(current);
    }

    return vertices;
}

function hexStringToInt(hexStr) {
    return parseInt(hexStr, 16);
}

function shoelace(vertices) {
    const n = vertices.length;
    let area = 0;

    for (let i = 0; i < n; i++) {
        const next = (i + 1) % n;
        area += vertices[i].x * vertices[next].y;
        area -= vertices[i].y * vertices[next].x;
    }

    area = abs(area) / 2;
    return area;
}

function perimeter(vertices) {
    const n = vertices.length;
    let perim = 0;

    for (let i = 0; i < n; i++) {
        const next = (i + 1) % n;
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y);
    }

    return perim;
}

function calculatePolygonArea(vertices) {
    return shoelace(vertices) + perimeter(vertices) / 2 + 1;
}

function solve(input) {
    const vertices = parseInput(input);

    return calculatePolygonArea(vertices);
}

function readFile(fileName) {
    const data = fs.readFileSync(fileName, 'utf8');
    return data.trim().split("\n");
}

const input = readFile("input.txt");
console.log(solve(input));