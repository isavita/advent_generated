const fs = require('fs');

function solve(input) {
    const { nums, boards } = parseInput(input);

    let lastWinningScore = -1;
    const alreadyWon = {};
    nums.forEach(n => {
        boards.forEach((b, bi) => {
            if (alreadyWon[bi]) {
                return;
            }
            const didWin = b.PickNum(n);
            if (didWin) {
                lastWinningScore = b.Score() * n;
                alreadyWon[bi] = true;
            }
        });
    });

    return lastWinningScore;
}

class BoardState {
    constructor(board) {
        this.board = board;
        this.picked = Array.from({ length: board.length }, () => Array.from({ length: board[0].length }, () => false));
    }

    PickNum(num) {
        this.board.forEach((rows, r) => {
            rows.forEach((v, c) => {
                if (v === num) {
                    this.picked[r][c] = true;
                }
            });
        });

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

    Score() {
        let score = 0;
        this.board.forEach((rows, r) => {
            rows.forEach((v, c) => {
                if (!this.picked[r][c]) {
                    score += v;
                }
            });
        });

        return score;
    }
}

function parseInput(input) {
    const lines = input.split("\n\n");
    const nums = lines[0].split(",").map(toInt);
    const boards = lines.slice(1).map(grid => {
        const b = grid.split("\n").map(line => {
            line = line.replace(/  /g, " ");
            while (line[0] === ' ') {
                line = line.slice(1);
            }
            const parts = line.split(" ");
            return parts.map(toInt);
        });
        return new BoardState(b);
    });

    return { nums, boards };
}

function toInt(s) {
    const n = parseInt(s);
    if (isNaN(n)) {
        throw new Error("Invalid number: " + s);
    }
    return n;
}

const input = fs.readFileSync("input.txt", "utf8").trim();
const result = solve(input);
console.log(result);