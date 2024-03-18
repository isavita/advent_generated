const fs = require('fs');

function solve(input) {
  const tiles = parseTilesFromInput(input);
  const edgeSize = Math.sqrt(tiles.length);

  const assembledTiles = backtrackAssemble(tiles, null, new Set());

  let product = assembledTiles[0][0].id;
  product *= assembledTiles[0][edgeSize - 1].id;
  product *= assembledTiles[edgeSize - 1][0].id;
  product *= assembledTiles[edgeSize - 1][edgeSize - 1].id;
  return product;
}

function parseTilesFromInput(input) {
  const tiles = [];
  for (const block of input.trim().split('\n\n')) {
    const lines = block.split('\n');
    const tileID = parseInt(lines[0].match(/Tile (\d+):/)[1]);
    const contents = lines.slice(1).map(line => line.split(''));
    tiles.push({ id: tileID, contents });
  }
  return tiles;
}

function backtrackAssemble(tiles, assembledTiles, usedIndices) {
  const edgeSize = Math.sqrt(tiles.length);
  if (!assembledTiles) {
    assembledTiles = Array.from({ length: edgeSize }, () => Array.from({ length: edgeSize }));
  }

  for (let row = 0; row < edgeSize; row++) {
    for (let col = 0; col < edgeSize; col++) {
      if (!assembledTiles[row][col]) {
        for (let i = 0; i < tiles.length; i++) {
          if (!usedIndices.has(i)) {
            for (const opt of allGridOrientations(tiles[i].contents)) {
              if (row !== 0 && getRow(opt, true) !== getRow(assembledTiles[row - 1][col].contents, false)) {
                continue;
              }
              if (col !== 0 && getCol(opt, true) !== getCol(assembledTiles[row][col - 1].contents, false)) {
                continue;
              }
              tiles[i].contents = opt;
              assembledTiles[row][col] = tiles[i];
              usedIndices.add(i);
              const recurseResult = backtrackAssemble(tiles, assembledTiles, usedIndices);
              if (recurseResult) {
                return recurseResult;
              }
              assembledTiles[row][col] = null;
              usedIndices.delete(i);
            }
          }
        }
        if (!assembledTiles[row][col]) {
          return null;
        }
      }
    }
  }

  return assembledTiles;
}

function getCol(grid, firstCol) {
  let str = '';
  for (let i = 0; i < grid.length; i++) {
    str += firstCol ? grid[i][0] : grid[i][grid[0].length - 1];
  }
  return str;
}

function getRow(grid, firstRow) {
  let str = '';
  for (let i = 0; i < grid[0].length; i++) {
    str += firstRow ? grid[0][i] : grid[grid.length - 1][i];
  }
  return str;
}

function removeBordersFromGrid(grid) {
  const result = [];
  for (let i = 1; i < grid.length - 1; i++) {
    result.push(grid[i].slice(1, grid[i].length - 1));
  }
  return result;
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
  const rotated = Array.from({ length: grid[0].length }, () => Array(grid.length));
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[0].length; j++) {
      rotated[grid[0].length - 1 - j][i] = grid[i][j];
    }
  }
  return rotated;
}

function mirrorStringGrid(grid) {
  return grid.map(row => row.slice().reverse());
}

const input = fs.readFileSync('input.txt', 'utf8');
const answer = solve(input);
console.log(answer);