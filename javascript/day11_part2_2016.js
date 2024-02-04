const fs = require('fs');

function main() {
    const input = fs.readFileSync('input.txt', 'utf8');
    const ans = rtgHellDay(input, 2);
    console.log(ans);
}

function rtgHellDay(input, part) {
    let currentState = newInitialState(input);

    if (part === 2) {
        currentState.floors[0].push(
            { isChip: false, material: "elerium" },
            { isChip: true, material: "elerium" },
            { isChip: false, material: "dilithium" },
            { isChip: true, material: "dilithium" }
        );
    }

    let queue = [currentState];
    let prevStates = {};
    while (queue.length > 0) {
        let front = queue[0];
        queue = queue.slice(1);

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

class Halves {
    constructor(isChip, material) {
        this.isChip = isChip;
        this.material = material;
    }

    toString() {
        let tType = this.isChip ? " microchip" : " generator";
        return `${this.material}${tType}`;
    }
}

class State {
    constructor() {
        this.floors = [[], [], [], []];
        this.elevatorLevel = 0;
        this.steps = 0;
    }

    hashKey() {
        let mapGenToIndex = {};
        let mapChipToIndex = {};
        this.floors.forEach((fl, flIndex) => {
            fl.forEach(half => {
                if (half.isChip) {
                    mapChipToIndex[half.material] = flIndex;
                } else {
                    mapGenToIndex[half.material] = flIndex;
                }
            });
        });

        let genChipPairs = [];
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

    isValid() {
        for (let i = 0; i < this.floors.length; i++) {
            let gensSeen = {};
            this.floors[i].forEach(half => {
                if (!half.isChip) {
                    gensSeen[half.material] = true;
                }
            });

            if (Object.keys(gensSeen).length === 0) {
                continue;
            }

            for (let half of this.floors[i]) {
                if (half.isChip && !gensSeen[half.material]) {
                    return false;
                }
            }
        }

        return true;
    }

    isDone() {
        let lenSum = 0;
        this.floors.slice(0, 3).forEach(fl => {
            lenSum += fl.length;
        });
        return lenSum === 0;
    }

    getMovablePermIndices() {
        let permsToMove = [];

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

    clone() {
        let cl = new State();
        cl.elevatorLevel = this.elevatorLevel;
        cl.steps = this.steps;

        this.floors.forEach((fl, i) => {
            cl.floors[i] = [...fl];
        });

        return cl;
    }

    getNextStates() {
        let futureStates = [];
        let movablePermIndices = this.getMovablePermIndices();

        let eleDiffs = [];
        if (this.elevatorLevel < this.floors.length - 1) {
            eleDiffs.push(1);
        }
        if (this.elevatorLevel > 0) {
            eleDiffs.push(-1);
        }

        eleDiffs.forEach(eleDiff => {
            movablePermIndices.forEach(permIndices => {
                let cl = this.clone();
                cl.elevatorLevel += eleDiff;
                cl.steps++;
                let oldLevel = this.elevatorLevel;
                let newLevel = cl.elevatorLevel;

                permIndices.forEach(index => {
                    cl.floors[newLevel].push(cl.floors[oldLevel][index]);
                });

                for (let i = permIndices.length - 1; i >= 0; i--) {
                    cl.floors[oldLevel][permIndices[i]] = cl.floors[oldLevel][cl.floors[oldLevel].length - 1];
                    cl.floors[oldLevel].pop();
                }

                if (cl.isValid()) {
                    futureStates.push(cl);
                }
            });
        });

        return futureStates;
    }
}

function newInitialState(input) {
    let s = new State();

    input.split('\n').forEach((line, lineIndex) => {
        let parts = line.split(' ').map(part => part.replace(/[,\.]/g, ''));

        parts.forEach((word, i) => {
            if (word === "generator") {
                let material = parts[i - 1];
                s.floors[lineIndex].push(new Halves(false, material));
            } else if (word === "microchip") {
                let material = parts[i - 1].substring(0, parts[i - 1].indexOf('-comp'));
                s.floors[lineIndex].push(new Halves(true, material));
            }
        });
    });

    return s;
}

main();