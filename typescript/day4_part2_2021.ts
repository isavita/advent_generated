import * as fs from 'fs';

type Board = {
    numbers: number[][];
    marked: boolean[][];
};

const readInput = (filename: string): { draws: number[], boards: Board[] } => {
    const data = fs.readFileSync(filename, 'utf-8').trim().split('\n');
    const draws = data[0].split(',').map(Number);
    const boards: Board[] = [];
    
    for (let i = 2; i < data.length; i += 6) {
        const board: Board = {
            numbers: [],
            marked: Array.from({ length: 5 }, () => Array(5).fill(false))
        };
        for (let j = 0; j < 5; j++) {
            board.numbers.push(data[i + j].trim().split(/\s+/).map(Number));
        }
        boards.push(board);
    }
    
    return { draws, boards };
};

const checkWin = (board: Board): boolean => {
    for (let i = 0; i < 5; i++) {
        if (board.marked[i].every(Boolean) || board.marked.map(row => row[i]).every(Boolean)) {
            return true;
        }
    }
    return false;
};

const calculateScore = (board: Board, lastDraw: number): number => {
    const unmarkedSum = board.numbers.flatMap((row, i) => 
        row.filter((_, j) => !board.marked[i][j])
    ).reduce((sum, num) => sum + num, 0);
    return unmarkedSum * lastDraw;
};

const findLastWinningBoard = (draws: number[], boards: Board[]): number => {
    const winningBoards = new Set<number>();

    for (const draw of draws) {
        for (let i = 0; i < boards.length; i++) {
            if (!winningBoards.has(i)) {
                const board = boards[i];
                for (let row = 0; row < 5; row++) {
                    for (let col = 0; col < 5; col++) {
                        if (board.numbers[row][col] === draw) {
                            board.marked[row][col] = true;
                        }
                    }
                }
                if (checkWin(board)) {
                    winningBoards.add(i);
                    if (winningBoards.size === boards.length) {
                        return calculateScore(board, draw);
                    }
                }
            }
        }
    }
    return 0;
};

const main = () => {
    const { draws, boards } = readInput('input.txt');
    const lastWinningScore = findLastWinningBoard(draws, boards);
    console.log(lastWinningScore);
};

main();