
<?php

function solve(array $input): int {
    $grid = buildGrid($input, '.');
    $expandedGrid = expandGrid($grid, 2);
    $coords = array_keys($expandedGrid['data']);
    $count = count($coords);
    $res = 0;

    for ($i = 0; $i < $count; $i++) {
        for ($j = $i + 1; $j < $count; $j++) {
            $res += calculateLength($expandedGrid, $coords[$i], $coords[$j]);
        }
    }
    return $res;
}

function buildGrid(array $input, string $empty): array {
    $height = count($input);
    $width = strlen($input[0]);
    $data = [];

    for ($y = 0; $y < $height; $y++) {
        for ($x = 0; $x < $width; $x++) {
            if ($input[$y][$x] !== $empty) {
                $data[$x . ',' . $y] = $input[$y][$x];
            }
        }
    }

    return ['width' => $width, 'height' => $height, 'data' => $data];
}

function getEmptyRows(array $grid): array {
    $emptyRows = [];
    for ($y = 0; $y < $grid['height']; $y++) {
        $isEmpty = true;
        for ($x = 0; $x < $grid['width']; $x++) {
            if (isset($grid['data'][$x . ',' . $y])) {
                $isEmpty = false;
                break;
            }
        }
        if ($isEmpty) {
            $emptyRows[] = $y;
        }
    }
    return $emptyRows;
}

function getEmptyCols(array $grid): array {
    $emptyCols = [];
    for ($x = 0; $x < $grid['width']; $x++) {
        $isEmpty = true;
        for ($y = 0; $y < $grid['height']; $y++) {
            if (isset($grid['data'][$x . ',' . $y])) {
                $isEmpty = false;
                break;
            }
        }
        if ($isEmpty) {
            $emptyCols[] = $x;
        }
    }
    return $emptyCols;
}

function calculateOffsets(array $emptyIndexes, int $bound): array {
    $offsets = array_fill(0, $bound, 0);
    foreach ($emptyIndexes as $idx) {
        for ($i = $idx + 1; $i < $bound; $i++) {
            $offsets[$i]++;
        }
    }
    return $offsets;
}

function expandGrid(array $grid, int $expansionFactor): array {
    $emptyCols = getEmptyCols($grid);
    $emptyRows = getEmptyRows($grid);
    $numLinesToAdd = $expansionFactor - 1;

    $newWidth = $grid['width'] + count($emptyCols) * $numLinesToAdd;
    $newHeight = $grid['height'] + count($emptyRows) * $numLinesToAdd;
    $newData = [];

    $dXs = calculateOffsets($emptyCols, $grid['width']);
    $dYs = calculateOffsets($emptyRows, $grid['height']);

    foreach ($grid['data'] as $coordStr => $value) {
        list($x, $y) = explode(',', $coordStr);
        $newX = (int)$x + $dXs[(int)$x] * $numLinesToAdd;
        $newY = (int)$y + $dYs[(int)$y] * $numLinesToAdd;
        $newData[$newX . ',' . $newY] = $value;
    }

    return ['width' => $newWidth, 'height' => $newHeight, 'data' => $newData];
}

function absVal(int $x): int {
    return $x < 0 ? -$x : $x;
}

function calculateLength(array $grid, string $c1, string $c2): int {
    list($x1, $y1) = explode(',', $c1);
    list($x2, $y2) = explode(',', $c2);
    $dX = absVal((int)$x2 - (int)$x1);
    $dY = absVal((int)$y2 - (int)$y1);
    return $dX + $dY;
}

$input = explode("\n", trim(file_get_contents("input.txt")));
echo solve($input) . PHP_EOL;
