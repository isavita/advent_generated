import * as fs from 'fs';

class BingoBoard {
    numbers: number[][];
    marked: boolean[][];

    constructor() {
        this.numbers = Array.from({ length: 5 }, () => Array(5).fill(0));
        this.marked = Array.from({ length: 5 }, () => Array(5).fill(false));
    }

    mark(number: number) {
        for (let i = 0; i < this.numbers.length; i++) {
            for (let j = 0; j < this.numbers[i].length; j++) {
                if (this.numbers[i][j] === number) {
                    this.marked[i][j] = true;
                }
            }
        }
    }

    hasWon(): boolean {
        for (let i = 0; i < this.marked.length; i++) {
            if (this.isRowMarked(this.marked[i]) || this.isColumnMarked(i)) {
                return true;
            }
        }
        return false;
    }

    unmarkedSum(): number {
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

    isRowMarked(row: boolean[]): boolean {
        return row.every(marked => marked);
    }

    isColumnMarked(column: number): boolean {
        return this.marked.every(row => row[column]);
    }
}

const fileContent = fs.readFileSync('input.txt', 'utf-8');
const lines = fileContent.split('\n');
const numbers = lines[0].split(',').map(Number);

const boards: BingoBoard[] = [];
for (let i = 2; i < lines.length; i += 6) {
    const board = new BingoBoard();
    for (let j = 0; j < 5; j++) {
        board.numbers[j] = lines[i + j].trim().split(/\s+/).map(Number);
    }
    boards.push(board);
}

let winningBoard: BingoBoard | null = null;
let winningNumber = 0;

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

if (winningBoard) {
    console.log(winningBoard.unmarkedSum() * winningNumber);
}