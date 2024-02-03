
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

    return $newGrid;
}

$file = fopen("input.txt", "r");
if ($file === false) {
    echo "Error opening file\n";
    exit;
}

$grid = array_fill(0, gridSize, array_fill(0, gridSize, false));

$y = 0;
while (($line = fgets($file)) !== false) {
    $line = str_split(trim($line));
    foreach ($line as $x => $c) {
        $grid[$x][$y] = $c === '#';
    }
    $y++;
}

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

fclose($file);
?>
