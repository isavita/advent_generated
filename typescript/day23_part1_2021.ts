import * as fs from 'fs';

type HeapNode = { value: number; state: State };
type State = {
    grid: string[][];
    energyUsed: number;
    path: string;
};

class MinHeap {
    private nodes: HeapNode[] = [];
    private closerToRoot: (val1: number, val2: number) => boolean;

    constructor() {
        this.closerToRoot = (val1, val2) => val1 < val2;
    }

    add(newNode: HeapNode) {
        this.nodes.push(newNode);
        this.heapifyFromEnd();
    }

    remove(): HeapNode | null {
        if (this.nodes.length === 0) return null;
        const rootNode = this.nodes[0];
        this.nodes[0] = this.nodes.pop()!;
        this.heapifyFromStart();
        return rootNode;
    }

    length(): number {
        return this.nodes.length;
    }

    private swap(i: number, j: number) {
        [this.nodes[i], this.nodes[j]] = [this.nodes[j], this.nodes[i]];
    }

    private heapifyFromEnd() {
        let currentIndex = this.nodes.length - 1;
        while (currentIndex > 0) {
            const parentIndex = Math.floor((currentIndex - 1) / 2);
            if (this.closerToRoot(this.nodes[currentIndex].value, this.nodes[parentIndex].value)) {
                this.swap(parentIndex, currentIndex);
                currentIndex = parentIndex;
            } else break;
        }
    }

    private heapifyFromStart() {
        let currentIndex = 0;
        while (true) {
            let smallerChildIndex = currentIndex;
            for (let i = 1; i <= 2; i++) {
                const childIndex = currentIndex * 2 + i;
                if (childIndex < this.nodes.length && this.closerToRoot(this.nodes[childIndex].value, this.nodes[smallerChildIndex].value)) {
                    smallerChildIndex = childIndex;
                }
            }
            if (smallerChildIndex === currentIndex) return;
            this.swap(smallerChildIndex, currentIndex);
            currentIndex = smallerChildIndex;
        }
    }
}

const roomCoordToWantChar: Record<string, string> = {
    '2,3': 'A', '3,3': 'A',
    '2,5': 'B', '3,5': 'B',
    '2,7': 'C', '3,7': 'C',
    '2,9': 'D', '3,9': 'D',
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const ans = amphipod(input);
    console.log(ans);
};

const amphipod = (input: string): number => {
    const start = parseInput(input);
    const minHeap = new MinHeap();
    minHeap.add({ value: start.energyUsed, state: start });
    const seenGrids = new Set<string>();

    while (minHeap.length() > 0) {
        const front = minHeap.remove() as HeapNode;
        const state = front.state;
        const key = JSON.stringify(state.grid);
        if (seenGrids.has(key)) continue;
        seenGrids.add(key);

        if (allDone(state)) return state.energyUsed;

        const unsettledCoords = getUnsettledCoords(state);
        for (const unsettledCoord of unsettledCoords) {
            const nextMoves = getNextPossibleMoves(state, unsettledCoord);
            for (const nextCoord of nextMoves) {
                const cp = copyState(state);
                cp.energyUsed += calcEnergy(cp.grid[unsettledCoord[0]][unsettledCoord[1]], unsettledCoord, nextCoord);
                cp.path += `${cp.grid[unsettledCoord[0]][unsettledCoord[1]]}${unsettledCoord}->${nextCoord}{${cp.energyUsed}},`;
                [cp.grid[nextCoord[0]][nextCoord[1]], cp.grid[unsettledCoord[0]][unsettledCoord[1]]] = [cp.grid[unsettledCoord[0]][unsettledCoord[1]], cp.grid[nextCoord[0]][nextCoord[1]]];
                minHeap.add({ value: cp.energyUsed, state: cp });
            }
        }
    }
    throw new Error("should return from loop");
};

const parseInput = (input: string): State => {
    const grid = input.split('\n').map(line => line.split(''));
    return { grid, energyUsed: 0, path: '' };
};

const allDone = (s: State): boolean => {
    return Object.entries(roomCoordToWantChar).every(([coord, want]) => {
        const [r, c] = coord.split(',').map(Number);
        return s.grid[r][c] === want;
    });
};

const getUnsettledCoords = (s: State): number[][] => {
    const unsettled: number[][] = [];
    for (let col = 1; col < s.grid[0].length; col++) {
        if ('ABCD'.includes(s.grid[1][col])) unsettled.push([1, col]);
    }
    for (const col of [3, 5, 7, 9]) {
        let roomFullFromBack = true;
        for (let row = s.grid.length - 2; row >= 2; row--) {
            const coord = [row, col];
            const wantChar = roomCoordToWantChar[`${row},${col}`];
            const gotChar = s.grid[row][col];
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
};

const getNextPossibleMoves = (s: State, unsettledCoord: number[]): number[][] => {
    const unsettledChar = s.grid[unsettledCoord[0]][unsettledCoord[1]];
    const possible: number[][] = [];
    const queue: number[][] = [unsettledCoord];
    const seen = new Set<string>();

    while (queue.length > 0) {
        const front = queue.shift()!;
        const key = front.join(',');
        if (seen.has(key)) continue;
        seen.add(key);

        if (front !== unsettledCoord) {
            if (!coordsInFrontOfRooms[`${front[0]},${front[1]}`]) {
                const wantChar = roomCoordToWantChar[`${front[0]},${front[1]}`];
                if (!wantChar || wantChar === unsettledChar) {
                    possible.push(front);
                }
            }
        }

        for (const d of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
            const next = [front[0] + d[0], front[1] + d[1]];
            if (s.grid[next[0]][next[1]] === '.') queue.push(next);
        }
    }
    return possible;
};

const copyState = (s: State): State => {
    const cp: State = {
        grid: s.grid.map(row => [...row]),
        energyUsed: s.energyUsed,
        path: s.path,
    };
    return cp;
};

const calcEnergy = (char: string, start: number[], end: number[]): number => {
    const dist = Math.abs(end[1] - start[1]) + start[0] - 1 + end[0] - 1;
    const energyPerType: Record<string, number> = { 'A': 1, 'B': 10, 'C': 100, 'D': 1000 };
    return energyPerType[char] * dist;
};

const coordsInFrontOfRooms: Record<string, boolean> = {
    '1,3': true,
    '1,5': true,
    '1,7': true,
    '1,9': true,
};

main();