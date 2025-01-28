
<?php

function readTilesFromFile(string $filename): array
{
    $tiles = [];
    $content = file_get_contents($filename);
    $tileBlocks = explode("\n\n", trim($content));

    foreach ($tileBlocks as $block) {
        $lines = explode("\n", $block);
        $tileIdLine = array_shift($lines);
        preg_match('/Tile (\d+):/', $tileIdLine, $matches);
        $tileId = intval($matches[1]);
        $pixels = [];
        foreach ($lines as $line) {
            $pixels[] = str_split($line);
        }
        $tiles[$tileId] = $pixels;
    }
    return $tiles;
}

function rotateTile(array $pixels): array
{
    $size = count($pixels);
    $rotated = array_fill(0, $size, array_fill(0, $size, ''));
    for ($i = 0; $i < $size; $i++) {
        for ($j = 0; $j < $size; $j++) {
            $rotated[$j][$size - 1 - $i] = $pixels[$i][$j];
        }
    }
    return $rotated;
}

function flipTile(array $pixels): array
{
    $flipped = [];
    foreach ($pixels as $row) {
        $flipped[] = array_reverse($row);
    }
    return $flipped;
}

function getBorders(array $pixels): array
{
    $size = count($pixels);
    $top = implode('', $pixels[0]);
    $bottom = implode('', $pixels[$size - 1]);
    $left = '';
    $right = '';
    for ($i = 0; $i < $size; $i++) {
        $left .= $pixels[$i][0];
        $right .= $pixels[$i][$size - 1];
    }
    return [$top, $right, $bottom, $left];
}

function getTransformations(array $pixels): array
{
    $transformations = [];
    $current = $pixels;
    for ($i = 0; $i < 4; $i++) {
        $transformations[] = $current;
        $transformations[] = flipTile($current);
        $current = rotateTile($current);
    }
    return $transformations;
}

function solvePuzzle(array $tilePixels): array
{
    $tiles = [];
    foreach ($tilePixels as $id => $pixels) {
        $tiles[$id] = [
            'id' => $id,
            'pixels' => $pixels,
            'transformations' => getTransformations($pixels),
        ];
    }

    $tileIds = array_keys($tiles);
    $gridSize = sqrt(count($tileIds));
    $placedTiles = array_fill(0, $gridSize, array_fill(0, $gridSize, null));
    $usedTileIds = [];

    function findArrangement(int $row, int $col, array &$placedTiles, array &$usedTileIds, array $tiles, int $gridSize): bool
    {
        if ($row === $gridSize) {
            return true; // Puzzle solved
        }

        $nextRow = $col === $gridSize - 1 ? $row + 1 : $row;
        $nextCol = $col === $gridSize - 1 ? 0 : $col + 1;

        foreach ($tiles as $tileId => $tileData) {
            if (!in_array($tileId, $usedTileIds)) {
                foreach ($tileData['transformations'] as $transformationIndex => $transformation) {
                    $validPlacement = true;
                    if ($row > 0) {
                        $topTile = $placedTiles[$row - 1][$col];
                        $topTileBorders = getBorders($topTile['pixels']);
                        $currentTileBorders = getBorders($transformation);
                        if ($topTileBorders[2] !== $currentTileBorders[0]) {
                            $validPlacement = false;
                        }
                    }
                    if ($validPlacement && $col > 0) {
                        $leftTile = $placedTiles[$row][$col - 1];
                        $leftTileBorders = getBorders($leftTile['pixels']);
                        $currentTileBorders = getBorders($transformation);
                        if ($leftTileBorders[1] !== $currentTileBorders[3]) {
                            $validPlacement = false;
                        }
                    }

                    if ($validPlacement) {
                        $placedTiles[$row][$col] = ['id' => $tileId, 'pixels' => $transformation];
                        $usedTileIds[] = $tileId;
                        if (findArrangement($nextRow, $nextCol, $placedTiles, $usedTileIds, $tiles, $gridSize)) {
                            return true;
                        }
                        array_pop($usedTileIds);
                        $placedTiles[$row][$col] = null; // Backtrack
                    }
                }
            }
        }
        return false;
    }

    findArrangement(0, 0, $placedTiles, $usedTileIds, $tiles, $gridSize);
    return $placedTiles;
}

function part1(array $assembledGrid): int
{
    $size = count($assembledGrid);
    $cornerProduct = 1;
    $cornerProduct *= $assembledGrid[0][0]['id'];
    $cornerProduct *= $assembledGrid[0][$size - 1]['id'];
    $cornerProduct *= $assembledGrid[$size - 1][0]['id'];
    $cornerProduct *= $assembledGrid[$size - 1][$size - 1]['id'];
    return $cornerProduct;
}

function removeBordersFromGrid(array $assembledGrid): array
{
    $fullImage = [];
    $tileSize = count($assembledGrid[0][0]['pixels']) - 2;
    $gridSize = count($assembledGrid);

    for ($gridRow = 0; $gridRow < $gridSize; $gridRow++) {
        for ($tileRow = 1; $tileRow <= $tileSize; $tileRow++) {
            $imageRow = [];
            for ($gridCol = 0; $gridCol < $gridSize; $gridCol++) {
                for ($tileCol = 1; $tileCol <= $tileSize; $tileCol++) {
                    $imageRow[] = $assembledGrid[$gridRow][$gridCol]['pixels'][$tileRow][$tileCol];
                }
            }
            $fullImage[] = $imageRow;
        }
    }
    return $fullImage;
}

function findSeaMonsters(array $image): int
{
    $monsterPattern = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    ];
    $monsterHeight = count($monsterPattern);
    $monsterWidth = strlen($monsterPattern[0]);
    $imageHeight = count($image);
    $imageWidth = count($image[0]);
    $monsterCount = 0;
    $monsterPositions = [];

    $transformations = getTransformations($image);

    foreach ($transformations as $transformedImage) {
        $currentMonsterPositions = [];
        for ($row = 0; $row <= $imageHeight - $monsterHeight; $row++) {
            for ($col = 0; $col <= $imageWidth - $monsterWidth; $col++) {
                $isMonster = true;
                $currentMonsterPixelPositions = [];
                for ($monsterRow = 0; $monsterRow < $monsterHeight; $monsterRow++) {
                    for ($monsterCol = 0; $monsterCol < $monsterWidth; $monsterCol++) {
                        if ($monsterPattern[$monsterRow][$monsterCol] === '#') {
                            if ($transformedImage[$row + $monsterRow][$col + $monsterCol] !== '#') {
                                $isMonster = false;
                                break 2;
                            }
                            $currentMonsterPixelPositions[] = [$row + $monsterRow, $col + $monsterCol];
                        }
                    }
                }
                if ($isMonster) {
                    $monsterCount++;
                    $currentMonsterPositions = array_merge($currentMonsterPositions, $currentMonsterPixelPositions);
                }
            }
        }
        if ($monsterCount > 0) {
            $monsterPositions = $currentMonsterPositions;
            break; // Found monsters, no need to check other transformations
        }
    }


    $hashCount = 0;
    foreach ($image as $row) {
        foreach ($row as $pixel) {
            if ($pixel === '#') {
                $hashCount++;
            }
        }
    }

    $monsterHashSet = [];
    foreach($monsterPositions as $pos) {
        $monsterHashSet[implode(',', $pos)] = true;
    }
    $monsterHashCount = count($monsterHashSet);


    return $hashCount - $monsterHashCount;
}


$tilePixels = readTilesFromFile("input.txt");
$assembledGrid = solvePuzzle($tilePixels);

$part1Answer = part1($assembledGrid);
echo "Part 1: " . $part1Answer . "\n";

$fullImage = removeBordersFromGrid($assembledGrid);
$part2Answer = findSeaMonsters($fullImage);
echo "Part 2: " . $part2Answer . "\n";

?>
