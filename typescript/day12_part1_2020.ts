import * as fs from 'fs';

enum Direction {
    East = 0,
    South = 90,
    West = 180,
    North = 270
}

const turn = (currentDirection: number, degrees: number): number => {
    return (currentDirection + degrees + 360) % 360;
};

const moveForward = (position: [number, number], direction: number, value: number): [number, number] => {
    switch (direction) {
        case Direction.East: return [position[0] + value, position[1]];
        case Direction.South: return [position[0], position[1] - value];
        case Direction.West: return [position[0] - value, position[1]];
        case Direction.North: return [position[0], position[1] + value];
        default: return position;
    }
};

const calculateManhattanDistance = (instructions: string[]): number => {
    let position: [number, number] = [0, 0];
    let direction = Direction.East;

    for (const instruction of instructions) {
        const action = instruction[0];
        const value = parseInt(instruction.slice(1));

        switch (action) {
            case 'N': position[1] += value; break;
            case 'S': position[1] -= value; break;
            case 'E': position[0] += value; break;
            case 'W': position[0] -= value; break;
            case 'L': direction = turn(direction, -value); break;
            case 'R': direction = turn(direction, value); break;
            case 'F': position = moveForward(position, direction, value); break;
        }
    }

    return Math.abs(position[0]) + Math.abs(position[1]);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const distance = calculateManhattanDistance(input);
    console.log(distance);
};

main();