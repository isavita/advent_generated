import * as fs from 'fs';

class MinHeap {
    nodes: HeapNode[] = [];
    closerToRoot: (val1: number, val2: number) => boolean;

    constructor() {
        this.closerToRoot = (val1, val2) => val1 < val2;
    }

    front(): HeapNode | null {
        return this.nodes.length === 0 ? null : this.nodes[0];
    }

    add(newNode: HeapNode): void {
        this.nodes.push(newNode);
        this.heapifyFromEnd();
    }

    remove(): HeapNode | null {
        if (this.nodes.length === 0) return null;

        const rootNode = this.nodes[0];
        this.nodes[0] = this.nodes[this.nodes.length - 1];
        this.nodes.pop();
        this.heapifyFromStart();

        return rootNode;
    }

    length(): number {
        return this.nodes.length;
    }

    swap(i: number, j: number): void {
        [this.nodes[i], this.nodes[j]] = [this.nodes[j], this.nodes[i]];
    }

    heapifyFromEnd(): void {
        let currentIndex = this.nodes.length - 1;
        while (currentIndex > 0) {
            const parentIndex = Math.floor((currentIndex - 1) / 2);
            if (this.closerToRoot(this.nodes[currentIndex].value(), this.nodes[parentIndex].value())) {
                this.swap(parentIndex, currentIndex);
                currentIndex = parentIndex;
            } else {
                break;
            }
        }
    }

    heapifyFromStart(): void {
        let currentIndex = 0;

        while (true) {
            let smallerChildIndex = currentIndex;
            for (let i = 1; i <= 2; i++) {
                const childIndex = currentIndex * 2 + i;
                if (childIndex < this.nodes.length &&
                    this.closerToRoot(this.nodes[childIndex].value(), this.nodes[smallerChildIndex].value())) {
                    smallerChildIndex = childIndex;
                }
            }

            if (smallerChildIndex === currentIndex) {
                return;
            }

            this.swap(smallerChildIndex, currentIndex);
            currentIndex = smallerChildIndex;
        }
    }
}

class MaxHeap extends MinHeap {
    constructor() {
        super();
        this.closerToRoot = (val1, val2) => val1 > val2;
    }
}

interface HeapNode {
    value(): number;
}

const roomCoordToWantChar: { [key: string]: string } = {
    '2,3': 'A', '3,3': 'A', '4,3': 'A', '5,3': 'A',
    '2,5': 'B', '3,5': 'B', '4,5': 'B', '5,5': 'B',
    '2,7': 'C', '3,7': 'C', '4,7': 'C', '5,7': 'C',
    '2,9': 'D', '3,9': 'D', '4,9': 'D', '5,9': 'D',
};

class State implements HeapNode {
    grid: string[][];
    energyUsed: number;
    path: string;

    constructor(grid: string[][], energyUsed = 0, path = '') {
        this.grid = grid;
        this.energyUsed = energyUsed;
        this.path = path;
    }

    value(): number {
        return this.energyUsed;
    }

    copy(): State {
        const cp = new State([...this.grid.map(row => [...row])], this.energyUsed, this.path);
        return cp;
    }

    allDone(roomCoordToWantChar: { [key: string]: string }): boolean {
        for (const coord in roomCoordToWantChar) {
            const [r, c] = coord.split(',').map(Number);
            if (this.grid[r][c] !== roomCoordToWantChar[coord]) {
                return false;
            }
        }
        return true;
    }

    getUnsettledCoords(roomCoordToWantChar: { [key: string]: string }): [number, number][] {
        const unsettled: [number, number][] = [];

        for (let col = 1; col < this.grid[0].length; col++) {
            if ('ABCD'.includes(this.grid[1][col])) {
                unsettled.push([1, col]);
            }
        }

        for (const col of [3, 5, 7, 9]) {
            let roomFullFromBack = true;
            for (let row = this.grid.length - 2; row >= 2; row--) {
                const coord: [number, number] = [row, col];
                const wantChar = roomCoordToWantChar[`${coord[0]},${coord[1]}`];
                const gotChar = this.grid[row][col];
                if (gotChar !== '.') {
                    if (gotChar !== wantChar) {
                        roomFullFromBack = false;
                        unsettled.push(coord);
                    } else if (gotChar === wantChar && !roomFullFromBack) {
                        unsettled.push(coord);
                    }
                }
            }
        }
        return unsettled;
    }

    getNextPossibleMoves(unsettledCoord: [number, number], roomCoordToWantChar: { [key: string]: string }): [number, number][] {
        const unsettledChar = this.grid[unsettledCoord[0]][unsettledCoord[1]];
        if (!'ABCD'.includes(unsettledChar)) {
            throw new Error(`Unexpected character to get next moves for ${unsettledChar}`);
        }

        const possible: [number, number][] = [];
        const startedInHallway = unsettledCoord[0] === 1;
        const queue: [number, number][] = [unsettledCoord];
        const seen: { [key: string]: boolean } = {};

        while (queue.length > 0) {
            const front = queue.shift()!;
            const key = `${front[0]},${front[1]}`;
            if (seen[key]) continue;
            seen[key] = true;

            if (front !== unsettledCoord) {
                if (!coordsInFrontOfRooms[key]) {
                    const wantChar = roomCoordToWantChar[key];
                    const isRoomCoord = wantChar !== undefined;

                    if (!isRoomCoord) {
                        if (!startedInHallway) {
                            possible.push(front);
                        }
                    } else if (wantChar === unsettledChar) {
                        let isStuckAmphipod = false;
                        let roomHasDeeperOpenSpaces = false;
                        for (let r = front[0] + 1; r < this.grid.length - 1; r++) {
                            const char = this.grid[r][front[1]];
                            if (char === '.') {
                                roomHasDeeperOpenSpaces = true;
                            }
                            if (char !== '.' && char !== unsettledChar) {
                                isStuckAmphipod = true;
                                break;
                            }
                        }

                        if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) {
                            possible.push(front);
                        }
                    }
                }
            }

            for (const d of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
                const next: [number, number] = [front[0] + d[0], front[1] + d[1]];
                if (this.grid[next[0]][next[1]] === '.') {
                    queue.push(next);
                }
            }
        }

        return possible;
    }
}

function parseInput(input: string): State {
    const grid = input.split('\n').map(line => line.split(''));
    return new State(grid);
}

function amphipod(input: string): number {
    const start = parseInput(input);
    start.grid.push([]);
    start.grid.push([]);
    start.grid[6] = start.grid[4];
    start.grid[5] = start.grid[3];
    start.grid[3] = "  #D#C#B#A#  ".split('');
    start.grid[4] = "  #D#B#A#C#  ".split('');

    const minHeap = new MinHeap();
    minHeap.add(start);
    const seenGrids: { [key: string]: boolean } = {};

    while (minHeap.length() > 0) {
        const front = minHeap.remove() as State;
        const key = JSON.stringify(front.grid);
        if (seenGrids[key]) continue;
        seenGrids[key] = true;

        if (front.allDone(roomCoordToWantChar)) {
            return front.energyUsed;
        }

        const unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar);
        for (const unsettledCoord of unsettledCoords) {
            const nextMoves = front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar);
            for (const nextCoord of nextMoves) {
                const [nr, nc] = nextCoord;
                if (front.grid[nr][nc] !== '.') {
                    throw new Error(`Should only be moving to walkable spaces, got ${front.grid[nr][nc]} at ${nr},${nc}`);
                }

                const cp = front.copy();
                cp.energyUsed += calcEnergy(cp.grid[unsettledCoord[0]][unsettledCoord[1]], unsettledCoord, nextCoord);
                cp.path += `${cp.grid[unsettledCoord[0]][unsettledCoord[1]]}${unsettledCoord}->${nextCoord}{${cp.energyUsed}},`;
                [cp.grid[nr][nc], cp.grid[unsettledCoord[0]][unsettledCoord[1]]] = [cp.grid[unsettledCoord[0]][unsettledCoord[1]], cp.grid[nr][nc]];

                minHeap.add(cp);
            }
        }
    }

    throw new Error('Should return from loop');
}

const coordsInFrontOfRooms: { [key: string]: boolean } = {
    '1,3': true,
    '1,5': true,
    '1,7': true,
    '1,9': true,
};

function calcEnergy(char: string, start: [number, number], end: [number, number]): number {
    const dist = Math.abs(end[1] - start[1]) + start[0] - 1 + end[0] - 1;
    const energyPerType: { [key: string]: number } = {
        'A': 1,
        'B': 10,
        'C': 100,
        'D': 1000,
    };
    return energyPerType[char] * dist;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim();
console.log(amphipod(input));