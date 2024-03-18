const fs = require('fs');

class BingoBoard {
  constructor() {
    this.numbers = [];
    this.marked = [];
  }

  mark(number) {
    for (let i = 0; i < this.numbers.length; i++) {
      for (let j = 0; j < this.numbers[i].length; j++) {
        if (this.numbers[i][j] === number) {
          this.marked[i][j] = true;
        }
      }
    }
  }

  hasWon() {
    for (let i = 0; i < this.marked.length; i++) {
      if (this.isRowMarked(this.marked[i]) || this.isColumnMarked(this.marked, i)) {
        return true;
      }
    }
    return false;
  }

  unmarkedSum() {
    let sum = 0;
    for (let i = 0; i < this.numbers.length; i++) {
      for (let j = 0; j < this.numbers[i].length; j++) {
        if (!this.marked[i][j]) {
          sum += this.numbers[i][j];
        }
      }
    }
    return sum;
  }

  isRowMarked(row) {
    for (let i = 0; i < row.length; i++) {
      if (!row[i]) {
        return false;
      }
    }
    return true;
  }

  isColumnMarked(marked, column) {
    for (let i = 0; i < marked.length; i++) {
      if (!marked[i][column]) {
        return false;
      }
    }
    return true;
  }
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const lines = data.trim().split('\n');
  const numbers = lines[0].split(',').map(Number);
  const boards = [];

  for (let i = 2; i < lines.length; i += 6) {
    const board = new BingoBoard();
    for (let j = 0; j < 5; j++) {
      board.numbers[j] = lines[i + j].trim().split(/\s+/).map(Number);
      board.marked[j] = new Array(5).fill(false);
    }
    boards.push(board);
  }

  let winningBoard, winningNumber;
  for (const number of numbers) {
    for (const board of boards) {
      board.mark(number);
      if (board.hasWon()) {
        winningBoard = board;
        winningNumber = number;
        break;
      }
    }
    if (winningBoard) {
      break;
    }
  }

  console.log(winningBoard.unmarkedSum() * winningNumber);
});