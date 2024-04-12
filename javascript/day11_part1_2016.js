const fs = require('fs');

function rtgHellDay(input) {
  let currentState = newInitialState(input);

  let queue = [currentState];
  let prevStates = new Set();
  while (queue.length > 0) {
    let front = queue.shift();

    if (front.isDone()) {
      return front.steps;
    }

    let hash = front.hashKey();
    if (prevStates.has(hash)) {
      continue;
    }
    prevStates.add(hash);

    let nextStates = front.getNextStates();
    queue = queue.concat(nextStates);
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
    for (let flIndex = 0; flIndex < this.floors.length; flIndex++) {
      for (let half of this.floors[flIndex]) {
        if (half.isChip) {
          mapChipToIndex[half.material] = flIndex;
        } else {
          mapGenToIndex[half.material] = flIndex;
        }
      }
    }

    let genChipPairs = [];
    for (let material in mapGenToIndex) {
      genChipPairs.push([
        mapGenToIndex[material], mapChipToIndex[material]
      ]);
    }

    genChipPairs.sort((a, b) => {
      if (a[0] !== b[0]) {
        return a[0] - b[0];
      }
      return a[1] - b[1];
    });

    return `${this.elevatorLevel}${JSON.stringify(genChipPairs)}`;
  }

  isValid() {
    for (let i = 0; i < this.floors.length; i++) {
      let gensSeen = new Set();
      for (let half of this.floors[i]) {
        if (!half.isChip) {
          gensSeen.add(half.material);
        }
      }

      if (gensSeen.size === 0) {
        continue;
      }

      for (let half of this.floors[i]) {
        if (half.isChip && !gensSeen.has(half.material)) {
          return false;
        }
      }
    }

    return true;
  }

  isDone() {
    let lenSum = 0;
    for (let i = 0; i < 3; i++) {
      lenSum += this.floors[i].length;
    }
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

    for (let i = 0; i < this.floors.length; i++) {
      cl.floors[i] = [...this.floors[i]];
    }
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

        for (let in_ = permIndices.length - 1; in_ >= 0; in_--) {
          cl.floors[oldLevel][permIndices[in_]] = cl.floors[oldLevel][cl.floors[oldLevel].length - 1];
          cl.floors[oldLevel].pop();
        }

        if (cl.isValid()) {
          futureStates.push(cl);
        }
      }
    }

    return futureStates;
  }
}

function newInitialState(input) {
  let s = new State();

  let lines = input.split("\n");
  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    let line = lines[lineIndex];
    let parts = line.split(" ").map(v => v.replace(/[,.]/g, ""));

    for (let i = 0; i < parts.length; i++) {
      let word = parts[i];
      if (word === "generator") {
        let material = parts[i - 1];
        s.floors[lineIndex].push(new Halves(false, material));
      } else if (word === "microchip") {
        let material = parts[i - 1].split("-comp")[0];
        s.floors[lineIndex].push(new Halves(true, material));
      }
    }
  }

  return s;
}

function main() {
  let input = fs.readFileSync("input.txt", "utf-8").trim();
  let ans = rtgHellDay(input);
  console.log(ans);
}

main();