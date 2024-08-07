import * as fs from 'fs';

const countVisibleTrees = (grid: number[][]): number => {
    const rows = grid.length;
    const cols = grid[0].length;
    let visibleCount = 0;

    const isVisible = (r: number, c: number): boolean => {
        const height = grid[r][c];
        const directions = [
            { dr: -1, dc: 0 }, // up
            { dr: 1, dc: 0 },  // down
            { dr: 0, dc: -1 }, // left
            { dr: 0, dc: 1 }   // right
        ];

        for (const { dr, dc } of directions) {
            let nr = r + dr, nc = c + dc;
            let blocked = false;

            while (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] >= height) {
                    blocked = true;
                    break;
                }
                nr += dr;
                nc += dc;
            }

            if (!blocked) return true;
        }
        return false;
    };

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (r === 0 || r === rows - 1 || c === 0 || c === cols - 1 || isVisible(r, c)) {
                visibleCount++;
            }
        }
    }

    return visibleCount;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const grid = input.trim().split('\n').map(line => line.split('').map(Number));
    const result = countVisibleTrees(grid);
    console.log(result);
};

main();