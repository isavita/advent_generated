const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const directions = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
];

const countOccupiedSeats = (seats, i, j) => {
  let count = 0;
  for (const [dx, dy] of directions) {
    let x = i + dx;
    let y = j + dy;
    while (x >= 0 && x < seats.length && y >= 0 && y < seats[0].length) {
      if (seats[x][y] === '#') {
        count++;
        break;
      }
      if (seats[x][y] === 'L') {
        break;
      }
      x += dx;
      y += dy;
    }
  }
  return count;
};

const applyRules = (seats) => {
  const newSeats = [];
  let changed = false;

  for (let i = 0; i < seats.length; i++) {
    let newRow = '';
    for (let j = 0; j < seats[0].length; j++) {
      const currentSeat = seats[i][j];
      if (currentSeat === 'L' && countOccupiedSeats(seats, i, j) === 0) {
        newRow += '#';
        changed = true;
      } else if (currentSeat === '#' && countOccupiedSeats(seats, i, j) >= 5) {
        newRow += 'L';
        changed = true;
      } else {
        newRow += currentSeat;
      }
    }
    newSeats.push(newRow);
  }

  return [changed, newSeats];
};

let changed = true;
let currentSeats = input;

while (changed) {
  [changed, currentSeats] = applyRules(currentSeats);
}

const occupiedSeats = currentSeats.join('').split('').filter(seat => seat === '#').length;

console.log(occupiedSeats);