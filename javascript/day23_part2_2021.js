const fs = require('fs');

class MinHeap {
  constructor() {
    this.heap = {
      nodes: [],
      closerToRoot: (val1, val2) => val1 < val2,
    };
  }

  Front() {
    if (this.heap.nodes.length === 0) {
      return null;
    }
    return this.heap.nodes[0];
  }

  Add(newNode) {
    this.heap.nodes.push(newNode);
    this.heapifyFromEnd();
  }

  Remove() {
    if (this.heap.nodes.length === 0) {
      return null;
    }

    const rootNode = this.heap.nodes[0];

    this.heap.nodes[0] = this.heap.nodes[this.heap.nodes.length - 1];
    this.heap.nodes.pop();

    this.heapifyFromStart();

    return rootNode;
  }

  Length() {
    return this.heap.nodes.length;
  }

  swap(i, j) {
    [this.heap.nodes[i], this.heap.nodes[j]] = [this.heap.nodes[j], this.heap.nodes[i]];
  }

  heapifyFromEnd() {
    let currentIndex = this.heap.nodes.length - 1;
    while (currentIndex > 0) {
      const parentIndex = Math.floor((currentIndex - 1) / 2);
      const parentNode = this.heap.nodes[parentIndex];
      if (this.heap.closerToRoot(this.heap.nodes[currentIndex].Value(), parentNode.Value())) {
        this.swap(parentIndex, currentIndex);
        currentIndex = parentIndex;
      } else {
        break;
      }
    }
  }

  heapifyFromStart() {
    let currentIndex = 0;

    while (true) {
      let smallerChildIndex = currentIndex;
      for (let i = 1; i <= 2; i++) {
        const childIndex = currentIndex * 2 + i;
        if (
          childIndex < this.heap.nodes.length &&
          this.heap.closerToRoot(this.heap.nodes[childIndex].Value(), this.heap.nodes[smallerChildIndex].Value())
        ) {
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
    this.heap.closerToRoot = (val1, val2) => val1 > val2;
  }
}

class State {
  constructor(grid, energyUsed, path) {
    this.grid = grid;
    this.energyUsed = energyUsed;
    this.path = path;
  }

  Value() {
    return this.energyUsed;
  }

  toString() {
    let sb = '';
    for (const row of this.grid) {
      sb += row.join('') + '\n';
    }
    sb += `nrg: ${this.energyUsed}, path: ${this.path}\n`;
    return sb;
  }

  copy() {
    const cp = new State(
      this.grid.map((row) => [...row]),
      this.energyUsed,
      this.path
    );
    return cp;
  }

  allDone(roomCoordToWantChar) {
    for (const [coord, want] of Object.entries(roomCoordToWantChar)) {
      const [row, col] = coord.split(',').map(Number);
      if (this.grid[row][col] !== want) {
        return false;
      }
    }
    return true;
  }

  getUnsettledCoords(roomCoordToWantChar) {
    const unsettled = [];
    for (let col = 1; col < this.grid[0].length; col++) {
      if ('ABCD'.includes(this.grid[1][col])) {
        unsettled.push([1, col]);
      }
    }

    for (const col of [3, 5, 7, 9]) {
      let roomFullFromBack = true;
      for (let row = this.grid.length - 2; row >= 2; row--) {
        const coord = `${row},${col}`;
        const wantChar = roomCoordToWantChar[coord];
        const gotChar = this.grid[row][col];
        if (gotChar !== '.') {
          if (gotChar !== wantChar) {
            roomFullFromBack = false;
            unsettled.push([row, col]);
          } else if (gotChar === wantChar && !roomFullFromBack) {
            unsettled.push([row, col]);
          }
        }
      }
    }
    return unsettled;
  }

  getNextPossibleMoves(unsettledCoord, roomCoordToWantChar) {
    const [row, col] = unsettledCoord;
    const unsettledChar = this.grid[row][col];

    if (!'ABCD'.includes(unsettledChar)) {
      throw new Error(`Unexpected character to get next moves for ${unsettledChar}`);
    }

    const possible = [];
    const startedInHallway = this.isInHallway(unsettledCoord);

    const queue = [unsettledCoord];
    const seen = new Set();
    while (queue.length > 0) {
      const [r, c] = queue.shift();
      if (seen.has(`${r},${c}`)) {
        continue;
      }
      seen.add(`${r},${c}`);

      if (r !== unsettledCoord[0] || c !== unsettledCoord[1]) {
        if (!this.coordsInFrontOfRooms[`${r},${c}`]) {
          const [wantChar, isRoomCoord] = this.getRoomCoordAndWantChar(roomCoordToWantChar, [r, c]);
          if (!isRoomCoord) {
            if (!startedInHallway) {
              possible.push([r, c]);
            }
          } else if (wantChar === unsettledChar) {
            let isStuckAmphipod = false;
            let roomHasDeeperOpenSpaces = false;
            for (let rr = r + 1; rr < this.grid.length - 1; rr++) {
              const char = this.grid[rr][c];
              if (char === '.') {
                roomHasDeeperOpenSpaces = true;
              }
              if (char !== '.' && char !== unsettledChar) {
                isStuckAmphipod = true;
                break;
              }
            }

            if (!roomHasDeeperOpenSpaces && !isStuckAmphipod) {
              possible.push([r, c]);
            }
          }
        }
      }

      for (const [dr, dc] of [[-1, 0], [1, 0], [0, -1], [0, 1]]) {
        const nr = r + dr;
        const nc = c + dc;
        if (this.grid[nr][nc] === '.') {
          queue.push([nr, nc]);
        }
      }
    }

    return possible;
  }

  isInHallway([row, _]) {
    return row === 1;
  }

  getRoomCoordAndWantChar(roomCoordToWantChar, [row, col]) {
    const coord = `${row},${col}`;
    if (coord in roomCoordToWantChar) {
      return [roomCoordToWantChar[coord], true];
    }
    return [null, false];
  }

  coordsInFrontOfRooms = {
    '1,3': true,
    '1,5': true,
    '1,7': true,
    '1,9': true,
  };
}

function calcEnergy(char, start, end) {
  const dist = Math.abs(end[1] - start[1]);
  const energyPerType = {
    A: 1,
    B: 10,
    C: 100,
    D: 1000,
  };
  if (!(char in energyPerType)) {
    throw new Error(`${char} should not call calcEnergy()`);
  }
  return energyPerType[char] * (dist + start[0] - 1 + end[0] - 1);
}

function amphipod(input) {
  const start = parseInput(input);

  start.grid.push(null, null);
  start.grid[6] = start.grid[4];
  start.grid[5] = start.grid[3];

  start.grid[3] = '  #D#C#B#A#  '.split('');
  start.grid[4] = '  #D#B#A#C#  '.split('');

  const minHeap = new MinHeap();
  minHeap.Add(start);
  const seenGrids = new Set();
  while (minHeap.Length() > 0) {
    const front = minHeap.Remove();
    const key = JSON.stringify(front.grid);
    if (seenGrids.has(key)) {
      continue;
    }
    seenGrids.add(key);

    if (front.allDone(roomCoordToWantChar)) {
      return front.energyUsed;
    }

    const unsettledCoords = front.getUnsettledCoords(roomCoordToWantChar);
    for (const unsettledCoord of unsettledCoords) {
      const [ur, uc] = unsettledCoord;
      const nextMoves = front.getNextPossibleMoves(unsettledCoord, roomCoordToWantChar);
      for (const [nr, nc] of nextMoves) {
        if (front.grid[nr][nc] !== '.') {
          throw new Error(`Should only be moving to walkable spaces, got ${front.grid[nr][nc]} at ${nr},${nc}`);
        }

        const cp = front.copy();
        cp.energyUsed += calcEnergy(cp.grid[ur][uc], unsettledCoord, [nr, nc]);
        cp.path += `${cp.grid[ur][uc]}${unsettledCoord}->${[nr, nc]}{${cp.energyUsed}},`;
        [cp.grid[nr][nc], cp.grid[ur][uc]] = [cp.grid[ur][uc], cp.grid[nr][nc]];
        minHeap.Add(cp);
      }
    }
  }

  throw new Error('Should return from loop');
}

function parseInput(input) {
  const grid = input.trim().split('\n').map((line) => line.split(''));
  return new State(grid, 0, '');
}

const roomCoordToWantChar = {
  '2,3': 'A', '3,3': 'A', '4,3': 'A', '5,3': 'A',
  '2,5': 'B', '3,5': 'B', '4,5': 'B', '5,5': 'B',
  '2,7': 'C', '3,7': 'C', '4,7': 'C', '5,7': 'C',
  '2,9': 'D', '3,9': 'D', '4,9': 'D', '5,9': 'D',
};

const input = fs.readFileSync('input.txt', 'utf8');
const answer = amphipod(input);
console.log(answer);