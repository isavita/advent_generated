<?php

function main() {
    $input = trim(file_get_contents("input.txt"));
    $ans = solve($input);
    echo $ans;
}

function solve($input) {
    $tiles = parseTilesFromInput($input);
    $edgeSize = (int)sqrt(count($tiles));

    $assembledTiles = backtrackAssemble($tiles, null, []);

    $product = $assembledTiles[0][0]->id;
    $product *= $assembledTiles[0][$edgeSize-1]->id;
    $product *= $assembledTiles[$edgeSize-1][0]->id;
    $product *= $assembledTiles[$edgeSize-1][$edgeSize-1]->id;
    return $product;
}

function parseTilesFromInput($input) {
    $ans = [];
    foreach (explode("\n\n", $input) as $block) {
        $split = explode("\n", $block);
        sscanf($split[0], "Tile %d:", $tileID);

        $contents = [];
        foreach (array_slice($split, 1) as $line) {
            $contents[] = str_split($line);
        }
        $ans[] = (object)['id' => $tileID, 'contents' => $contents];
    }
    return $ans;
}

function backtrackAssemble($tiles, $assembledTiles, $usedIndices) {
    $edgeSize = (int)sqrt(count($tiles));
    if ($assembledTiles === null) {
        $assembledTiles = array_fill(0, $edgeSize, array_fill(0, $edgeSize, null));
    }

    for ($row = 0; $row < $edgeSize; $row++) {
        for ($col = 0; $col < $edgeSize; $col++) {
            if ($assembledTiles[$row][$col] === null) {
                foreach ($tiles as $i => $t) {
                    if (empty($usedIndices[$i])) {
                        foreach (allGridOrientations($t->contents) as $opt) {
                            if ($row != 0) {
                                $currentTopRow = getRow($opt, true);
                                $bottomOfAbove = getRow($assembledTiles[$row-1][$col]->contents, false);
                                if ($currentTopRow !== $bottomOfAbove) continue;
                            }
                            if ($col != 0) {
                                $currentLeftCol = getCol($opt, true);
                                $rightColOfLeft = getCol($assembledTiles[$row][$col-1]->contents, false);
                                if ($currentLeftCol !== $rightColOfLeft) continue;
                            }

                            $t->contents = $opt;
                            $assembledTiles[$row][$col] = $t;

                            $usedIndices[$i] = true;
                            $recurseResult = backtrackAssemble($tiles, $assembledTiles, $usedIndices);
                            if ($recurseResult !== null) return $recurseResult;

                            $assembledTiles[$row][$col] = null;
                            unset($usedIndices[$i]);
                        }
                    }
                }
                return null;
            }
        }
    }
    return $assembledTiles;
}

function getCol($grid, $firstCol) {
    $str = '';
    foreach ($grid as $row) {
        $str .= $firstCol ? $row[0] : end($row);
    }
    return $str;
}

function getRow($grid, $firstRow) {
    $str = '';
    foreach ($grid[$firstRow ? 0 : count($grid)-1] as $char) {
        $str .= $char;
    }
    return $str;
}

function allGridOrientations($grid) {
    $orientations = [$grid];

    for ($i = 0; $i < 3; $i++) {
        $orientations[] = rotateStringGrid(end($orientations));
    }

    foreach ($orientations as $orientation) {
        $orientations[] = mirrorStringGrid($orientation);
    }

    return $orientations;
}

function rotateStringGrid($grid) {
    $rotated = [];
    for ($i = 0; $i < count($grid[0]); $i++) {
        $rotated[$i] = array_column(array_reverse($grid), $i);
    }
    return $rotated;
}

function mirrorStringGrid($grid) {
    $flipped = [];
    foreach ($grid as $row) {
        $flipped[] = array_reverse($row);
    }
    return $flipped;
}

main();