import * as fs from 'fs';

enum NodeState {
    Clean = '.',
    Infected = '#',
    Weakened = 'W',
    Flagged = 'F'
}

type Direction = [number, number];
const directions: Direction[] = [
    [0, -1], // Up
    [1, 0],  // Right
    [0, 1],  // Down
    [-1, 0]  // Left
];

class VirusCarrier {
    private grid: Map<string, NodeState>;
    private x: number;
    private y: number;
    private direction: number;
    private infections: number;

    constructor(initialMap: string[]) {
        this.grid = new Map();
        this.x = Math.floor(initialMap.length / 2);
        this.y = Math.floor(initialMap.length / 2);
        this.direction = 0; // Start facing up
        this.infections = 0;

        for (let row = 0; row < initialMap.length; row++) {
            for (let col = 0; col < initialMap[row].length; col++) {
                if (initialMap[row][col] === '#') {
                    this.grid.set(`${col},${row}`, NodeState.Infected);
                }
            }
        }
    }

    private turnLeft() {
        this.direction = (this.direction + 3) % 4; // Turn left
    }

    private turnRight() {
        this.direction = (this.direction + 1) % 4; // Turn right
    }

    private reverse() {
        this.direction = (this.direction + 2) % 4; // Reverse direction
    }

    public burst() {
        const currentNode = this.grid.get(`${this.x},${this.y}`) || NodeState.Clean;

        switch (currentNode) {
            case NodeState.Clean:
                this.turnLeft();
                this.grid.set(`${this.x},${this.y}`, NodeState.Weakened);
                break;
            case NodeState.Weakened:
                this.grid.set(`${this.x},${this.y}`, NodeState.Infected);
                this.infections++;
                break;
            case NodeState.Infected:
                this.turnRight();
                this.grid.set(`${this.x},${this.y}`, NodeState.Flagged);
                break;
            case NodeState.Flagged:
                this.reverse();
                this.grid.set(`${this.x},${this.y}`, NodeState.Clean);
                break;
        }

        this.x += directions[this.direction][0];
        this.y += directions[this.direction][1];
    }

    public getInfectionCount() {
        return this.infections;
    }
}

function readInput(filePath: string): string[] {
    return fs.readFileSync(filePath, 'utf-8').trim().split('\n');
}

function main() {
    const initialMap = readInput('input.txt');
    const virusCarrier = new VirusCarrier(initialMap);

    for (let i = 0; i < 10_000_000; i++) {
        virusCarrier.burst();
    }

    console.log(virusCarrier.getInfectionCount());
}

main();