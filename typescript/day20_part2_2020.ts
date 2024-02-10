
const fs = require('fs');

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    const ans = solve(input);
    console.log(ans);
}

function solve(input) {
    const tiles = parseTilesFromInput(input);
    const edgeSize = Math.sqrt(tiles.length);

    let assembledTiles = backtrackAssemble(tiles, null, new Map());

    for (let r = 0; r < assembledTiles.length; r++) {
        for (let c = 0; c < assembledTiles[0].length; c++) {
            assembledTiles[r][c].contents = removeBordersFromGrid(assembledTiles[r][c].contents);
        }
    }

    let image = [];
    for (let bigRow = 0; bigRow < edgeSize; bigRow++) {
        for (let subRow = 0; subRow < assembledTiles[0][0].contents.length; subRow++) {
            image.push([]);
            for (let bigCol = 0; bigCol < edgeSize; bigCol++) {
                const subLine = assembledTiles[bigRow][bigCol].contents[subRow];
                image[image.length - 1].push(...subLine);
            }
        }
    }

    let monsterCoords = [];
    for (const opt of allGridOrientations(image)) {
        monsterCoords = findMonsterCoords(opt);
        if (monsterCoords.length > 0) {
            image = opt;
            break;
        }
    }

    for (const coord of monsterCoords) {
        image[coord[0]][coord[1]] = "O";
    }

    let roughWatersCount = 0;
    for (const row of image) {
        for (const cell of row) {
            if (cell === "#") {
                roughWatersCount++;
            }
        }
    }

    return roughWatersCount;
}

function parseTilesFromInput(input) {
    const ans = [];
    for (const block of input.split("\n\n")) {
        const split = block.split("\n");
        let tileID;
        [, tileID] = split[0].match(/Tile (\d+):/) || [];
        
        const contents = [];
        for (const line of split.slice(1)) {
            contents.push(line.split(""));
        }
        ans.push({ id: parseInt(tileID), contents });
    }
    return ans;
}

function backtrackAssemble(tiles, assembledTiles, usedIndices) {
    const edgeSize = Math.sqrt(tiles.length);
    if (assembledTiles === null) {
        assembledTiles = new Array(edgeSize).fill(null).map(() => new Array(edgeSize).fill(null));
    }

    for (let row = 0; row < edgeSize; row++) {
        for (let col = 0; col < edgeSize; col++) {

            if (assembledTiles[row][col] === null) {

                for (let i = 0; i < tiles.length; i++) {
                    if (!usedIndices.get(i)) {

                        for (const opt of allGridOrientations(tiles[i].contents)) {

                            if (row !== 0) {
                                const currentTopRow = getRow(opt, true);
                                const bottomOfAbove = getRow(assembledTiles[row - 1][col].contents, false);

                                if (currentTopRow !== bottomOfAbove) {
                                    continue;
                                }
                            }
                            if (col !== 0) {
                                const currentLeftCol = getCol(opt, true);
                                const rightColOfLeft = getCol(assembledTiles[row][col - 1].contents, false);
                                if (currentLeftCol !== rightColOfLeft) {
                                    continue;
                                }
                            }

                            tiles[i].contents = opt;
                            assembledTiles[row][col] = tiles[i];

                            usedIndices.set(i, true);
                            const recurseResult = backtrackAssemble(tiles, assembledTiles, usedIndices);
                            if (recurseResult !== null) {
                                return recurseResult;
                            }

                            assembledTiles[row][col] = null;
                            usedIndices.set(i, false);
                        }
                    }
                }

                if (assembledTiles[row][col] === null) {
                    return null;
                }
            }
        }
    }

    return assembledTiles;
}

function getCol(grid, firstCol) {
    let str = "";
    for (let i = 0; i < grid.length; i++) {
        str += firstCol ? grid[i][0] : grid[i][grid[0].length - 1];
    }
    return str;
}

function getRow(grid, firstRow) {
    let str = "";
    for (let i = 0; i < grid[0].length; i++) {
        str += firstRow ? grid[0][i] : grid[grid.length - 1][i];
    }
    return str;
}

function removeBordersFromGrid(grid) {
    const result = [];

    for (let i = 1; i < grid.length - 1; i++) {
        result.push([]);
        for (let j = 1; j < grid[0].length - 1; j++) {
            result[i - 1].push(grid[i][j]);
        }
    }

    return result;
}

const monster = `                  # 
#    ##    ##    ###
 #  #  #  #  #  #   `;

function findMonsterCoords(image) {
    const monsterOffsets = [];
    let monsterHeight = 0, monsterLength = 0;
    for (const [r, line] of monster.split("\n").entries()) {
        for (const [c, char] of line.split("").entries()) {
            if (char === '#') {
                monsterOffsets.push([r, c]);
            }
            monsterLength = c + 1;
        }
        monsterHeight++;
    }

    const monsterStartingCoords = [];
    for (let r = 0; r < image.length - monsterHeight + 1; r++) {
        for (let c = 0; c < image[0].length - monsterLength + 1; c++) {
            let monsterFound = true;
            for (const diff of monsterOffsets) {
                const rowToCheck = r + diff[0];
                const colToCheck = c + diff[1];
                if (image[rowToCheck][colToCheck] !== "#") {
                    monsterFound = false;
                }
            }
            if (monsterFound) {
                monsterStartingCoords.push([r, c]);
            }
        }
    }

    const monsterCoords = [];
    for (const startingCoord of monsterStartingCoords) {
        for (const diff of monsterOffsets) {
            monsterCoords.push([startingCoord[0] + diff[0], startingCoord[1] + diff[1]]);
        }
    }

    return monsterCoords;
}

function allGridOrientations(grid) {
    const orientations = [grid];

    for (let i = 0; i < 3; i++) {
        orientations.push(rotateStringGrid(orientations[orientations.length - 1]));
    }

    for (let i = 0; i < 4; i++) {
        orientations.push(mirrorStringGrid(orientations[i]));
    }

    return orientations;
}

function rotateStringGrid(grid) {
    const rotated = new Array(grid[0].length).fill(null).map(() => new Array(grid.length).fill(""));
    
    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[0].length; j++) {
            rotated[grid[0].length - 1 - j][i] = grid[i][j];
        }
    }
    return rotated;
}

function mirrorStringGrid(grid) {
    const flipped = [];
    for (let i = 0; i < grid.length; i++) {
        flipped.push([]);
        for (let j = grid[i].length - 1; j >= 0; j--) {
            flipped[i].push(grid[i][j]);
        }
    }
    return flipped;
}

main();
