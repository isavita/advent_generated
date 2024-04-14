<?php

function readInput($filename) {
    $file = fopen($filename, "r");
    if (!$file) {
        throw new Exception("File cannot be opened.");
    }

    $grid = [];
    while (($line = fgets($file)) !== false) {
        $row = array_map('intval', str_split(trim($line)));
        $grid[] = $row;
    }

    fclose($file);
    return $grid;
}

function simulateStep(&$grid) {
    $flashes = 0;
    $flashed = [];

    // Increase energy by 1 for all octopuses
    foreach ($grid as $y => &$row) {
        foreach ($row as $x => &$value) {
            $value++;
        }
    }

    // Flash octopuses with energy greater than 9
    foreach ($grid as $y => &$row) {
        foreach ($row as $x => &$value) {
            if ($value > 9 && !isset($flashed[$y][$x])) {
                $flashes += flash($grid, $x, $y, $flashed);
            }
        }
    }

    // Reset energy to 0 for all that flashed
    foreach ($flashed as $y => $cols) {
        foreach (array_keys($cols) as $x) {
            $grid[$y][$x] = 0;
        }
    }

    return $flashes;
}

function flash(&$grid, $x, $y, &$flashed) {
    if (isset($flashed[$y][$x])) {
        return 0;
    }

    $flashed[$y][$x] = true;
    $flashes = 1;
    $directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];

    foreach ($directions as $dir) {
        $newX = $x + $dir[0];
        $newY = $y + $dir[1];
        if ($newX >= 0 && $newX < count($grid[0]) && $newY >= 0 && $newY < count($grid)) {
            $grid[$newY][$newX]++;
            if ($grid[$newY][$newX] > 9) {
                $flashes += flash($grid, $newX, $newY, $flashed);
            }
        }
    }

    return $flashes;
}

try {
    $grid = readInput("input.txt");
    $totalFlashes = 0;

    for ($step = 0; $step < 100; $step++) {
        $totalFlashes += simulateStep($grid);
    }

    echo $totalFlashes . "\n";
} catch (Exception $e) {
    echo "Error: " . $e->getMessage() . "\n";
}

?>