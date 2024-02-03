
<?php

$input = file_get_contents("input.txt");
echo solve($input);

function solve($input) {
    $matrix = parseInput($input);
    $originCol = 0;
    foreach ($matrix[0] as $i => $c) {
        if ($c == "+") {
            $originCol = $i;
        }
        $matrix[count($matrix) - 1][$i] = "#";
    }

    $ans = 0;
    while (!dropSand($matrix, $originCol)) {
        $ans++;
        if ($matrix[0][$originCol] == "o") {
            break;
        }
    }

    return $ans;
}

function parseInput($input) {
    $coordSets = [];
    $lowestCol = PHP_INT_MAX;
    $highestRow = 0;
    $lines = explode("\n", trim($input));
    foreach ($lines as $line) {
        $rawCoords = explode(" -> ", $line);
        $coords = [];
        foreach ($rawCoords as $rawCoord) {
            $rawNums = explode(",", $rawCoord);
            $col = (int)$rawNums[0];
            $row = (int)$rawNums[1];
            $coord = [$col, $row];
            $coords[] = $coord;

            $lowestCol = min($lowestCol, $col);
            $highestRow = max($highestRow, $row);
        }
        $coordSets[] = $coords;
    }

    $ExtraLeftSpace = 200;
    $highestCol = 0;
    foreach ($coordSets as $s => $set) {
        foreach ($set as $i => $coord) {
            $coordSets[$s][$i][0] -= $lowestCol - $ExtraLeftSpace;
            $highestCol = max($highestCol, $coordSets[$s][$i][0]);
        }
    }

    $matrix = [];
    for ($r = 0; $r < $highestRow + 3; $r++) {
        $matrix[$r] = array_fill(0, $highestCol + $ExtraLeftSpace * 2, ".");
    }

    foreach ($coordSets as $set) {
        for ($i = 1; $i < count($set); $i++) {
            $cols = [$set[$i - 1][0], $set[$i][0]];
            $rows = [$set[$i - 1][1], $set[$i][1]];

            sort($cols);
            sort($rows);

            if ($cols[0] == $cols[1]) {
                for ($r = $rows[0]; $r <= $rows[1]; $r++) {
                    $matrix[$r][$cols[0]] = "#";
                }
            } elseif ($rows[0] == $rows[1]) {
                for ($c = $cols[0]; $c <= $cols[1]; $c++) {
                    $matrix[$rows[0]][$c] = "#";
                }
            }
        }
    }

    $originCol = 500 - $lowestCol + $ExtraLeftSpace;
    $matrix[0][$originCol] = "+";

    return $matrix;
}

function dropSand(&$matrix, $originCol) {
    $r = 0;
    $c = $originCol;

    while ($r < count($matrix) - 1) {
        $below = $matrix[$r + 1][$c];
        $diagonallyLeft = $matrix[$r + 1][$c - 1];
        $diagonallyRight = $matrix[$r + 1][$c + 1];
        if ($below == ".") {
            $r++;
        } elseif ($diagonallyLeft == ".") {
            $r++;
            $c--;
        } elseif ($diagonallyRight == ".") {
            $r++;
            $c++;
        } else {
            $matrix[$r][$c] = "o";
            return false;
        }
    }

    return true;
}
