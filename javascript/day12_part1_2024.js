
const fs = require('fs');

function solve() {
    const grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const rows = grid.length;
    if (rows === 0) return 0;
    const cols = grid[0].length;

    const visited = Array(rows).fill(null).map(() => Array(cols).fill(false));
    let totalPrice = 0;

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (!visited[r][c]) {
                const [area, perimeter] = calculateRegion(grid, r, c, visited);
                totalPrice += area * perimeter;
            }
        }
    }
    return totalPrice;
}

function calculateRegion(grid, row, col, visited) {
    const rows = grid.length;
    const cols = grid[0].length;
    const char = grid[row][col];
    let area = 0;
    let perimeter = 0;
    const queue = [{ x: row, y: col }];
    visited[row][col] = true;

    while (queue.length > 0) {
        const { x, y } = queue.shift();
        area++;
        let isBorder = x === 0 || x === rows - 1 || y === 0 || y === cols - 1;

        const neighbors = [
            { dx: -1, dy: 0 },
            { dx: 1, dy: 0 },
            { dx: 0, dy: -1 },
            { dx: 0, dy: 1 }
        ];

        for (const { dx, dy } of neighbors) {
            const nx = x + dx;
            const ny = y + dy;

            if (nx >= 0 && nx < rows && ny >= 0 && ny < cols) {
                if (grid[nx][ny] !== char) {
                    perimeter++;
                } else if (!visited[nx][ny]) {
                    queue.push({ x: nx, y: ny });
                    visited[nx][ny] = true;
                }
            } else if (isBorder) {
                perimeter++;
            }
        }
    }
    return [area, perimeter];
}

console.log(solve());
