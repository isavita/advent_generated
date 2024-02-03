
<?php

$grid = readInput("input.txt");
$step = 0;

while (true) {
    $step++;
    $flashes = simulateStep($grid);
    if ($flashes == 100) {
        break;
    }
}

echo $step . PHP_EOL;

function readInput($filename) {
    $file = fopen($filename, "r");
    $grid = [];

    while (!feof($file)) {
        $line = trim(fgets($file));
        $row = str_split($line);
        $row = array_map('intval', $row);
        $grid[] = $row;
    }

    fclose($file);
    return $grid;
}

function simulateStep(&$grid) {
    $flashes = 0;
    $flashed = [];

    foreach ($grid as $y => $row) {
        foreach ($row as $x => $value) {
            $grid[$y][$x]++;
        }
    }

    foreach ($grid as $y => $row) {
        foreach ($row as $x => $value) {
            if ($grid[$y][$x] > 9) {
                $flashes += flash($grid, $x, $y, $flashed);
            }
        }
    }

    foreach ($flashed as $coords => $value) {
        list($x, $y) = explode(",", $coords);
        $grid[$y][$x] = 0;
    }

    return $flashes;
}

function flash(&$grid, $x, $y, &$flashed) {
    if (isset($flashed["$x,$y"])) {
        return 0;
    }

    $flashed["$x,$y"] = true;
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
?>
