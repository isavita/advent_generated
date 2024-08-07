import * as fs from 'fs';

interface Halves {
    isChip: boolean;
    material: string;
}

interface State {
    floors: Halves[][];
    elevatorLevel: number;
    steps: number;
    isDone: () => boolean;
    hashKey: () => string;
    isValid: () => boolean;
    getMovablePermIndices: () => number[][];
    clone: () => State;
    getNextStates: () => State[];
}

function rtgHellDay(input: string): number {
    let currentState = newInitialState(input);
    currentState.floors[0].push(
        { isChip: false, material: "elerium" },
        { isChip: true, material: "elerium" },
        { isChip: false, material: "dilithium" },
        { isChip: true, material: "dilithium" },
    );

    let queue: State[] = [currentState];
    let prevStates: { [key: string]: boolean } = {};

    while (queue.length > 0) {
        let front = queue.shift()!;

        if (front.isDone()) {
            return front.steps;
        }

        let hash = front.hashKey();
        if (prevStates[hash]) {
            continue;
        }
        prevStates[hash] = true;

        let nextStates = front.getNextStates();
        queue.push(...nextStates);
    }

    return -1;
}

function newInitialState(input: string): State {
    let s: State = {
        floors: [[], [], [], []],
        elevatorLevel: 0,
        steps: 0,
        isDone: isDone,
        hashKey: hashKey,
        isValid: isValid,
        getMovablePermIndices: getMovablePermIndices,
        clone: clone,
        getNextStates: getNextStates
    };

    input.split('\n').forEach((line, lineIndex) => {
        let parts = line.split(' ').map(v => v.trim().replace(/,|\./g, ''));

        parts.forEach((word, i) => {
            if (word === 'generator') {
                let material = parts[i - 1];
                s.floors[lineIndex].push({ isChip: false, material });
            } else if (word === 'microchip') {
                let material = parts[i - 1].replace(/-comp.*/, '');
                s.floors[lineIndex].push({ isChip: true, material });
            }
        });
    });

    return s;
}

function hashKey(this: State): string {
    let mapGenToIndex: { [key: string]: number } = {};
    let mapChipToIndex: { [key: string]: number } = {};

    this.floors.forEach((fl, flIndex) => {
        fl.forEach(half => {
            if (half.isChip) {
                mapChipToIndex[half.material] = flIndex;
            } else {
                mapGenToIndex[half.material] = flIndex;
            }
        });
    });

    let genChipPairs: [number, number][] = [];
    for (let material in mapGenToIndex) {
        genChipPairs.push([mapGenToIndex[material], mapChipToIndex[material]]);
    }

    genChipPairs.sort((a, b) => {
        if (a[0] !== b[0]) {
            return a[0] - b[0];
        }
        return a[1] - b[1];
    });

    return `${this.elevatorLevel}${genChipPairs}`;
}

function isValid(this: State): boolean {
    for (let fl of this.floors) {
        let gensSeen: { [key: string]: boolean } = {};
        for (let half of fl) {
            if (!half.isChip) {
                gensSeen[half.material] = true;
            }
        }

        if (Object.keys(gensSeen).length === 0) {
            continue;
        }

        for (let half of fl) {
            if (half.isChip && !gensSeen[half.material]) {
                return false;
            }
        }
    }

    return true;
}

function isDone(this: State): boolean {
    return this.floors.slice(0, 3).reduce((sum, fl) => sum + fl.length, 0) === 0;
}

function getMovablePermIndices(this: State): number[][] {
    let permsToMove: number[][] = [];
    let currentLevel = this.floors[this.elevatorLevel];

    for (let i = 0; i < currentLevel.length; i++) {
        for (let j = i + 1; j < currentLevel.length; j++) {
            permsToMove.push([i, j]);
        }
    }

    for (let i = 0; i < currentLevel.length; i++) {
        permsToMove.push([i]);
    }

    return permsToMove;
}

function clone(this: State): State {
    let cl: State = {
        elevatorLevel: this.elevatorLevel,
        steps: this.steps,
        floors: this.floors.map(fl => [...fl]),
        isDone: this.isDone,
        hashKey: this.hashKey,
        isValid: this.isValid,
        getMovablePermIndices: this.getMovablePermIndices,
        clone: this.clone,
        getNextStates: this.getNextStates
    };

    return cl;
}

function getNextStates(this: State): State[] {
    let futureStates: State[] = [];
    let movablePermIndices = this.getMovablePermIndices();
    let eleDiffs: number[] = [];

    if (this.elevatorLevel < this.floors.length - 1) {
        eleDiffs.push(1);
    }
    if (this.elevatorLevel > 0) {
        eleDiffs.push(-1);
    }

    for (let eleDiff of eleDiffs) {
        for (let permIndices of movablePermIndices) {
            let cl = this.clone();
            cl.elevatorLevel += eleDiff;
            cl.steps++;
            let oldLevel = this.elevatorLevel;
            let newLevel = cl.elevatorLevel;

            for (let index of permIndices) {
                cl.floors[newLevel].push(cl.floors[oldLevel][index]);
            }

            for (let inx = permIndices.length - 1; inx >= 0; inx--) {
                cl.floors[oldLevel][permIndices[inx]] = cl.floors[oldLevel][cl.floors[oldLevel].length - 1];
                cl.floors[oldLevel].pop();
            }

            if (cl.isValid()) {
                futureStates.push(cl);
            }
        }
    }

    return futureStates;
}

function readFile(path: string): string {
    return fs.readFileSync(path, 'utf-8').trim();
}

const input = readFile('./input.txt');
console.log(rtgHellDay(input));