const fs = require('fs');

function solve(input) {
  const tiles = parseTilesFromInput(input);
  const edgeSize = Math.sqrt(tiles.length);

  const assembledTiles = backtrackAssemble(tiles, null, {});

  for (let r in assembledTiles) {
    for (let c in assembledTiles[r]) {
      assembledTiles[r][c].contents = removeBordersFromGrid(assembledTiles[r][c].contents);
    }
  }

  let image = [];
  for (let bigRow = 0; bigRow < edgeSize; bigRow++) {
    for (let subRow = 0; subRow < assembledTiles[0][0].contents.length; subRow++) {
      image.push([]);
      for (let bigCol = 0; bigCol < edgeSize; bigCol++) {
        image[image.length - 1] = image[image.length - 1].concat(assembledTiles[bigRow][bigCol].contents[subRow]);
      }
    }
  }

  let monsterCoords = [];
  for (let opt of allGridOrientations(image)) {
    monsterCoords = findMonsterCoords(opt);
    if (monsterCoords.length > 0) {
      image = opt;
      break;
    }
  }

  for (let coord of monsterCoords) {
    image[coord[0]][coord[1]] = 'O';
  }

  let roughWatersCount = 0;
  for (let row of image) {
    for (let cell of row) {
      if (cell === '#') {
        roughWatersCount++;
      }
    }
  }

  return roughWatersCount;
}

function parseTilesFromInput(input) {
  const tiles = [];
  for (let block of input.split('\n\n')) {
    const split = block.split('\n');
    let tileID;
    [, tileID] = split[0].match(/Tile (\d+):/);
    tileID = parseInt(tileID);

    const contents = split.slice(1).map(line => line.split(''));
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
          if (!usedIndices[i]) {
            for (let opt of allGridOrientations(tiles[i].contents)) {
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

              usedIndices[i] = true;
              const recurseResult = backtrackAssemble(tiles, assembledTiles, usedIndices);
              if (recurseResult) {
                return recurseResult;
              }

              assembledTiles[row][col] = null;
              usedIndices[i] = false;
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

const monster = `                  # 
#    ##    ##    ###
 #  #  #  #  #  #   `;

function findMonsterCoords(image) {
  const monsterOffsets = [];
  let monsterHeight = 0, monsterLength = 0;
  for (let r = 0; r < monster.split('\n').length; r++) {
    for (let c = 0; c < monster.split('\n')[r].length; c++) {
      if (monster.split('\n')[r][c] === '#') {
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
      for (let [dr, dc] of monsterOffsets) {
        const rowToCheck = r + dr;
        const colToCheck = c + dc;
        if (image[rowToCheck][colToCheck] !== '#') {
          monsterFound = false;
          break;
        }
      }
      if (monsterFound) {
        monsterStartingCoords.push([r, c]);
      }
    }
  }

  const monsterCoords = [];
  for (let [r, c] of monsterStartingCoords) {
    for (let [dr, dc] of monsterOffsets) {
      monsterCoords.push([r + dr, c + dc]);
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
  const rotated = Array.from({ length: grid[0].length }, () => Array(grid.length));
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
    flipped.push(grid[i].slice().reverse());
  }
  return flipped;
}

const input = fs.readFileSync('input.txt', 'utf8');
const answer = solve(input);
console.log(answer);