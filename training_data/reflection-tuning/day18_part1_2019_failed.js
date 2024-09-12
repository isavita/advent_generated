function shortestPathAllKeys(grid) {
    // Parse the grid into a 2D array
    grid = grid.split('\n').map(row => row.split(''));
    const m = grid.length, n = grid[0].length;
    
    // Function to find coordinates of a character
    function findChar(char) {
        for (let i = 0; i < m; i++) {
            for (let j = 0; j < n; j++) {
                if (grid[i][j] === char) return [i, j];
            }
        }
        return [-1, -1];
    }

    // Find start position and count keys
    const start = findChar('@');
    let totalKeys = 0;
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (grid[i][j] >= 'a' && grid[i][j] <= 'f') totalKeys++;
        }
    }

    const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];
    const queue = [[...start, '', 0]]; // [row, col, collected keys, steps]
    const visited = new Set();

    while (queue.length > 0) {
        const [row, col, keys, steps] = queue.shift();
        const state = `${row},${col},${keys}`;
        
        if (visited.has(state)) continue;
        visited.add(state);

        if (keys.length === totalKeys) return steps;

        for (const [dx, dy] of directions) {
            const newRow = row + dx, newCol = col + dy;
            if (newRow < 0 || newRow >= m || newCol < 0 || newCol >= n || grid[newRow][newCol] === '#') continue;

            const cell = grid[newRow][newCol];
            if (cell >= 'A' && cell <= 'F' && !keys.includes(cell.toLowerCase())) continue;

            let newKeys = keys;
            if (cell >= 'a' && cell <= 'f' && !keys.includes(cell)) {
                newKeys = [...keys, cell].sort().join('');
            }

            queue.push([newRow, newCol, newKeys, steps + 1]);
        }
    }

    return -1; // If no path found
}

// Test the function
console.log(shortestPathAllKeys([
    "#########",
    "#b.A.@.a#",
    "#########"
].join('\n'))); // Expected output: 8
