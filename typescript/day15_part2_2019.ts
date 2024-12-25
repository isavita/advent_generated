
import * as fs from 'fs';

type Point = { x: number; y: number };
type Map = { [key: string]: number };

const NORTH = 1;
const SOUTH = 2;
const WEST = 3;
const EAST = 4;

const WALL = 0;
const MOVED = 1;
const OXYGEN = 2;

function pointToString(p: Point): string {
    return `${p.x},${p.y}`;
}

function stringToPoint(s: string): Point {
    const [x, y] = s.split(',').map(Number);
    return { x, y };
}

function getAdjacentPoints(p: Point): Point[] {
    return [
        { x: p.x, y: p.y - 1 }, // North
        { x: p.x, y: p.y + 1 }, // South
        { x: p.x - 1, y: p.y }, // West
        { x: p.x + 1, y: p.y }, // East
    ];
}

function runIntcode(program: number[], input: number[]): { output: number[], program: number[] } {
    let ip = 0;
    let relativeBase = 0;
    const output: number[] = [];
    let inputIndex = 0;

    const getParam = (mode: number, offset: number): number => {
        const value = program[ip + offset];
        switch (mode) {
            case 0: return program[value] || 0;
            case 1: return value;
            case 2: return program[relativeBase + value] || 0;
            default: throw new Error(`Invalid mode: ${mode}`);
        }
    };

    const setParam = (mode: number, offset: number, value: number): void => {
        const address = program[ip + offset];
        switch (mode) {
            case 0: program[address] = value; break;
            case 2: program[relativeBase + address] = value; break;
            default: throw new Error(`Invalid mode for set: ${mode}`);
        }
    };

    while (program[ip] !== 99) {
        const instruction = program[ip];
        const opcode = instruction % 100;
        const mode1 = Math.floor(instruction / 100) % 10;
        const mode2 = Math.floor(instruction / 1000) % 10;
        const mode3 = Math.floor(instruction / 10000) % 10;

        switch (opcode) {
            case 1:
                setParam(mode3, 3, getParam(mode1, 1) + getParam(mode2, 2));
                ip += 4;
                break;
            case 2:
                setParam(mode3, 3, getParam(mode1, 1) * getParam(mode2, 2));
                ip += 4;
                break;
            case 3:
                if (inputIndex >= input.length) {
                    return { output, program };
                }
                setParam(mode1, 1, input[inputIndex++]);
                ip += 2;
                break;
            case 4:
                output.push(getParam(mode1, 1));
                ip += 2;
                break;
            case 5:
                if (getParam(mode1, 1) !== 0) {
                    ip = getParam(mode2, 2);
                } else {
                    ip += 3;
                }
                break;
            case 6:
                if (getParam(mode1, 1) === 0) {
                    ip = getParam(mode2, 2);
                } else {
                    ip += 3;
                }
                break;
            case 7:
                setParam(mode3, 3, getParam(mode1, 1) < getParam(mode2, 2) ? 1 : 0);
                ip += 4;
                break;
            case 8:
                setParam(mode3, 3, getParam(mode1, 1) === getParam(mode2, 2) ? 1 : 0);
                ip += 4;
                break;
            case 9:
                relativeBase += getParam(mode1, 1);
                ip += 2;
                break;
            default:
                throw new Error(`Invalid opcode: ${opcode}`);
        }
    }
    return { output, program };
}

function exploreMap(program: number[]): { map: Map, oxygenSystemLocation: Point } {
    const map: Map = {};
    let currentPosition: Point = { x: 0, y: 0 };
    let oxygenSystemLocation: Point = { x: -1, y: -1 };
    const visited: Set<string> = new Set();
    const stack: { position: Point, path: number[] }[] = [{ position: currentPosition, path: [] }];

    while (stack.length > 0) {
        const { position, path } = stack.pop()!;
        currentPosition = position;
        const currentPositionKey = pointToString(currentPosition);

        if (visited.has(currentPositionKey)) {
            continue;
        }
        visited.add(currentPositionKey);

        for (let direction = 1; direction <= 4; direction++) {
            const { output } = runIntcode([...program], [...path, direction]);
            const status = output[output.length - 1];
            let nextPosition: Point;

            switch (direction) {
                case NORTH: nextPosition = { x: currentPosition.x, y: currentPosition.y - 1 }; break;
                case SOUTH: nextPosition = { x: currentPosition.x, y: currentPosition.y + 1 }; break;
                case WEST: nextPosition = { x: currentPosition.x - 1, y: currentPosition.y }; break;
                case EAST: nextPosition = { x: currentPosition.x + 1, y: currentPosition.y }; break;
                default: throw new Error(`Invalid direction: ${direction}`);
            }

            map[pointToString(nextPosition)] = status;

            if (status === OXYGEN) {
                oxygenSystemLocation = nextPosition;
            }

            if (status !== WALL) {
                stack.push({ position: nextPosition, path: [...path, direction] });
            }
        }
    }
    return { map, oxygenSystemLocation };
}

function findShortestPath(map: Map, start: Point, end: Point): number {
    const queue: { position: Point, distance: number }[] = [{ position: start, distance: 0 }];
    const visited: Set<string> = new Set();

    while (queue.length > 0) {
        const { position, distance } = queue.shift()!;
        const positionKey = pointToString(position);

        if (positionKey === pointToString(end)) {
            return distance;
        }

        if (visited.has(positionKey)) {
            continue;
        }
        visited.add(positionKey);

        const adjacentPoints = getAdjacentPoints(position);
        for (const nextPosition of adjacentPoints) {
            if (map[pointToString(nextPosition)] !== WALL) {
                queue.push({ position: nextPosition, distance: distance + 1 });
            }
        }
    }
    return -1;
}

function timeToFill(map: Map, oxygenSystemLocation: Point): number {
    let minutes = 0;
    let oxygenated: Set<string> = new Set([pointToString(oxygenSystemLocation)]);
    const allOpenLocations = Object.keys(map).filter(key => map[key] !== WALL);

    while (oxygenated.size < allOpenLocations.length) {
        minutes++;
        const newOxygenated: Set<string> = new Set(oxygenated);
        for (const oxygenatedLocation of oxygenated) {
            const adjacentPoints = getAdjacentPoints(stringToPoint(oxygenatedLocation));
            for (const nextPosition of adjacentPoints) {
                const nextPositionKey = pointToString(nextPosition);
                if (map[nextPositionKey] !== WALL) {
                    newOxygenated.add(nextPositionKey);
                }
            }
        }
        oxygenated = newOxygenated;
    }
    return minutes;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const program = input.trim().split(',').map(Number);

    const { map, oxygenSystemLocation } = exploreMap(program);
    const shortestPath = findShortestPath(map, { x: 0, y: 0 }, oxygenSystemLocation);
    const fillTime = timeToFill(map, oxygenSystemLocation);

    console.log(`Part 1: Shortest path to oxygen system: ${shortestPath}`);
    console.log(`Part 2: Time to fill with oxygen: ${fillTime}`);
}

main();
