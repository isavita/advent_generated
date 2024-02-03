
<?php

const gridSize = 100;
const steps = 100;

function countOnNeighbors($grid, $x, $y) {
    $on = 0;
    for ($dx = -1; $dx <= 1; $dx++) {
        for ($dy = -1; $dy <= 1; $dy++) {
            if ($dx == 0 && $dy == 0) {
                continue;
            }
            $nx = $x + $dx;
            $ny = $y + $dy;
            if ($nx >= 0 && $nx < gridSize && $ny >= 0 && $ny < gridSize && $grid[$nx][$ny]) {
                $on++;
            }
        }
    }
    return $on;
}

function step($grid) {
    $newGrid = array_fill(0, gridSize, array_fill(0, gridSize, false));

    for ($x = 0; $x < gridSize; $x++) {
        for ($y = 0; $y < gridSize; $y++) {
            $onNeighbors = countOnNeighbors($grid, $x, $y);
            if ($grid[$x][$y]) {
                $newGrid[$x][$y] = $onNeighbors == 2 || $onNeighbors == 3;
            } else {
                $newGrid[$x][$y] = $onNeighbors == 3;
            }
        }
    }

    // Ensure corners are always on
    $newGrid[0][0] = true;
    $newGrid[0][gridSize - 1] = true;
    $newGrid[gridSize - 1][0] = true;
    $newGrid[gridSize - 1][gridSize - 1] = true;

    return $newGrid;
}

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$grid = array_fill(0, gridSize, array_fill(0, gridSize, false));

$y = 0;
foreach ($lines as $line) {
    for ($x = 0; $x < strlen($line); $x++) {
        $grid[$x][$y] = $line[$x] === '#';
    }
    $y++;
}

// Initialize corners as always on
$grid[0][0] = true;
$grid[0][gridSize - 1] = true;
$grid[gridSize - 1][0] = true;
$grid[gridSize - 1][gridSize - 1] = true;

for ($i = 0; $i < steps; $i++) {
    $grid = step($grid);
}

$onCount = 0;
foreach ($grid as $row) {
    foreach ($row as $light) {
        if ($light) {
            $onCount++;
        }
    }
}

echo $onCount . "\n";
?>
