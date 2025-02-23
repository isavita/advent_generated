
import * as fs from 'fs';
import * as path from 'path';

interface Tile {
    id: number;
    contents: string[][];
}

function parseTilesFromInput(inputStr: string): Tile[] {
    return inputStr.split("\n\n").map(block => {
        const lines = block.split("\n");
        const tileId = parseInt(lines[0].split(" ")[1].slice(0, -1));
        const contents = lines.slice(1).map(line => line.split(""));
        return { id: tileId, contents: contents };
    });
}

function getCol(grid: string[][], firstCol: boolean): string {
    return grid.map(row => firstCol ? row[0] : row[row.length - 1]).join("");
}

function getRow(grid: string[][], firstRow: boolean): string {
    return (firstRow ? grid[0] : grid[grid.length - 1]).join("");
}

function removeBordersFromGrid(grid: string[][]): string[][] {
    return grid.slice(1, -1).map(row => row.slice(1, -1));
}

function allGridOrientations(grid: string[][]): string[][][] {
    const orientations: string[][][] = [grid];
    for (let i = 0; i < 3; i++) {
        orientations.push(rotateStringGrid(orientations[orientations.length - 1]));
    }
    for (let i = 0; i < 4; i++) {
        orientations.push(mirrorStringGrid(orientations[i]));
    }
    return orientations;
}

function rotateStringGrid(grid: string[][]): string[][] {
    const rows = grid.length;
    const cols = grid[0].length;
    const newGrid: string[][] = Array(cols).fill(null).map(() => Array(rows).fill(""));
    for (let i = 0; i < rows; i++) {
        for (let j = 0; j < cols; j++) {
            newGrid[j][rows - 1 - i] = grid[i][j];
        }
    }
    return newGrid;
}

function mirrorStringGrid(grid: string[][]): string[][] {
    return grid.map(row => [...row].reverse());
}

function findMonsterCoords(image: string[][]): [number, number][] {
    const monster = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ];
    const monsterOffsets: [number, number][] = [];
    for (let r = 0; r < monster.length; r++) {
        for (let c = 0; c < monster[r].length; c++) {
            if (monster[r][c] === "#") {
                monsterOffsets.push([r, c]);
            }
        }
    }
    const monsterHeight = monster.length;
    const monsterLength = monster[0].length;

    const monsterStartingCoords: [number, number][] = [];
    for (let r = 0; r < image.length - monsterHeight + 1; r++) {
        for (let c = 0; c < image[0].length - monsterLength + 1; c++) {
            let monsterFound = true;
            for (const [dr, dc] of monsterOffsets) {
                if (image[r + dr][c + dc] !== "#") {
                    monsterFound = false;
                    break;
                }
            }
            if (monsterFound) {
                monsterStartingCoords.push([r, c]);
            }
        }
    }

    const monsterCoords: [number, number][] = [];
    for (const [r, c] of monsterStartingCoords) {
        for (const [dr, dc] of monsterOffsets) {
            monsterCoords.push([r + dr, c + dc]);
        }
    }

    return monsterCoords;
}

function solve(inputStr: string): number {
    const tiles = parseTilesFromInput(inputStr);
    const edgeSize = Math.sqrt(tiles.length);

    function backtrackAssemble(
        assembledTiles: (Tile | null)[][],
        usedIndices: Set<number>
    ): (Tile | null)[][] | null {
        for (let row = 0; row < edgeSize; row++) {
            for (let col = 0; col < edgeSize; col++) {
                if (assembledTiles[row][col] === null) {
                    for (let i = 0; i < tiles.length; i++) {
                        if (!usedIndices.has(i)) {
                            for (const opt of allGridOrientations(tiles[i].contents)) {
                                if (row !== 0) {
                                    const currentTopRow = getRow(opt, true);
                                    const bottomOfAbove = getRow(assembledTiles[row - 1][col]!.contents, false);
                                    if (currentTopRow !== bottomOfAbove) {
                                        continue;
                                    }
                                }
                                if (col !== 0) {
                                    const currentLeftCol = getCol(opt, true);
                                    const rightColOfLeft = getCol(assembledTiles[row][col - 1]!.contents, false);
                                    if (currentLeftCol !== rightColOfLeft) {
                                        continue;
                                    }
                                }

                                const originalContents = tiles[i].contents;
                                tiles[i].contents = opt;
                                assembledTiles[row][col] = tiles[i];
                                usedIndices.add(i);
                                const result = backtrackAssemble(assembledTiles, usedIndices);
                                if (result !== null) {
                                    return result;
                                }
                                assembledTiles[row][col] = null;
                                usedIndices.delete(i);
                                tiles[i].contents = originalContents;
                            }
                        }
                    }
                    return null;
                }
            }
        }
        return assembledTiles;
    }

    let assembledTiles: (Tile | null)[][] = Array(edgeSize).fill(null).map(() => Array(edgeSize).fill(null));
    assembledTiles = backtrackAssemble(assembledTiles, new Set<number>())!;


    for (let row = 0; row < edgeSize; row++) {
        for (let col = 0; col < edgeSize; col++) {
            assembledTiles[row][col]!.contents = removeBordersFromGrid(assembledTiles[row][col]!.contents);
        }
    }

    const image: string[][] = [];
    for (let bigRow = 0; bigRow < edgeSize; bigRow++) {
        for (let subRow = 0; subRow < assembledTiles[0][0]!.contents.length; subRow++) {
            image.push([]);
            for (let bigCol = 0; bigCol < edgeSize; bigCol++) {
                const subLine = assembledTiles[bigRow][bigCol]!.contents[subRow];
                image[image.length - 1].push(...subLine);
            }
        }
    }

    let finalImage: string[][] = image;
    for (const opt of allGridOrientations(image)) {
        const monsterCoords = findMonsterCoords(opt);
        if (monsterCoords.length > 0) {
            finalImage = opt;
            break;
        }
    }

    const monsterCoords = findMonsterCoords(finalImage);
    for (const [r, c] of monsterCoords) {
        finalImage[r][c] = "O";
    }

    let roughWatersCount = 0;
    for (const row of finalImage) {
        for (const cell of row) {
            if (cell === "#") {
                roughWatersCount++;
            }
        }
    }

    return roughWatersCount;
}

const filePath = path.join(__dirname, 'input.txt');
const inputStr = fs.readFileSync(filePath, 'utf-8').trim();

console.log(solve(inputStr));
