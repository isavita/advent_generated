const fs = require('fs');

class BoardState {
  constructor(board) {
    this.board = board;
    this.picked = new Array(board.length).fill(null).map(() => new Array(board[0].length).fill(false));
  }

  pickNum(num) {
    for (let r = 0; r < this.board.length; r++) {
      for (let c = 0; c < this.board[0].length; c++) {
        if (this.board[r][c] === num) {
          this.picked[r][c] = true;
        }
      }
    }

    for (let i = 0; i < this.board.length; i++) {
      let isFullRow = true;
      let isFullCol = true;

      for (let j = 0; j < this.board.length; j++) {
        if (!this.picked[i][j]) {
          isFullRow = false;
        }

        if (!this.picked[j][i]) {
          isFullCol = false;
        }
      }

      if (isFullRow || isFullCol) {
        return true;
      }
    }

    return false;
  }

  score() {
    let score = 0;

    for (let r = 0; r < this.board.length; r++) {
      for (let c = 0; c < this.board[0].length; c++) {
        if (!this.picked[r][c]) {
          score += this.board[r][c];
        }
      }
    }

    return score;
  }
}

function solve(input) {
  const { nums, boards } = parseInput(input);

  let lastWinningScore = -1;
  const alreadyWon = {};

  for (const n of nums) {
    for (let bi = 0; bi < boards.length; bi++) {
      if (alreadyWon[bi]) {
        continue;
      }

      const didWin = boards[bi].pickNum(n);
      if (didWin) {
        lastWinningScore = boards[bi].score() * n;
        alreadyWon[bi] = true;
      }
    }
  }

  return lastWinningScore;
}

function parseInput(input) {
  const lines = input.split('\n\n');
  const nums = lines[0].split(',').map(toInt);
  const boards = [];

  for (const grid of lines.slice(1)) {
    const board = grid.split('\n')
      .map(line => line.trim().replace(/  /g, ' ').split(' ').map(toInt));

    boards.push(new BoardState(board));
  }

  return { nums, boards };
}

function toInt(s) {
  return parseInt(s, 10);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    throw err;
  }

  const input = data.trim();
  const result = solve(input);
  console.log(result);
});