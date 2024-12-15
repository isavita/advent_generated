
<?php

function solve() {
    $grid = file('input.txt', FILE_IGNORE_NEW_LINES);
    $start = findStart($grid);
    $loop = findLoop($grid, $start);
    echo max(array_values($loop)) . PHP_EOL;
}

function findStart($grid) {
    foreach ($grid as $y => $row) {
        $x = strpos($row, 'S');
        if ($x !== false) {
            return ['x' => $x, 'y' => $y];
        }
    }
    return null;
}

function findLoop($grid, $start) {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $distances = [$start['y'] . ',' . $start['x'] => 0];
    $queue = [$start];

    while (!empty($queue)) {
        $current = array_shift($queue);
        $x = $current['x'];
        $y = $current['y'];
        $current_char = $grid[$y][$x];
        $current_dist = $distances[$y . ',' . $x];

        $neighbors = getNeighbors($grid, $x, $y, $current_char);

        foreach ($neighbors as $neighbor) {
            $nx = $neighbor['x'];
            $ny = $neighbor['y'];
            $key = $ny . ',' . $nx;
            if (!isset($distances[$key])) {
                $distances[$key] = $current_dist + 1;
                $queue[] = ['x' => $nx, 'y' => $ny];
            }
        }
    }
    return $distances;
}

function getNeighbors($grid, $x, $y, $char) {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $neighbors = [];

    switch ($char) {
        case '|':
            if ($y > 0) $neighbors[] = ['x' => $x, 'y' => $y - 1];
            if ($y < $rows - 1) $neighbors[] = ['x' => $x, 'y' => $y + 1];
            break;
        case '-':
            if ($x > 0) $neighbors[] = ['x' => $x - 1, 'y' => $y];
            if ($x < $cols - 1) $neighbors[] = ['x' => $x + 1, 'y' => $y];
            break;
        case 'L':
            if ($y > 0) $neighbors[] = ['x' => $x, 'y' => $y - 1];
            if ($x < $cols - 1) $neighbors[] = ['x' => $x + 1, 'y' => $y];
            break;
        case 'J':
            if ($y > 0) $neighbors[] = ['x' => $x, 'y' => $y - 1];
            if ($x > 0) $neighbors[] = ['x' => $x - 1, 'y' => $y];
            break;
        case '7':
            if ($y < $rows - 1) $neighbors[] = ['x' => $x, 'y' => $y + 1];
            if ($x > 0) $neighbors[] = ['x' => $x - 1, 'y' => $y];
            break;
        case 'F':
            if ($y < $rows - 1) $neighbors[] = ['x' => $x, 'y' => $y + 1];
            if ($x < $cols - 1) $neighbors[] = ['x' => $x + 1, 'y' => $y];
            break;
        case 'S':
            // Check all directions for valid connections
            if ($y > 0 && in_array($grid[$y - 1][$x], ['|', '7', 'F'])) $neighbors[] = ['x' => $x, 'y' => $y - 1];
            if ($y < $rows - 1 && in_array($grid[$y + 1][$x], ['|', 'L', 'J'])) $neighbors[] = ['x' => $x, 'y' => $y + 1];
            if ($x > 0 && in_array($grid[$y][$x - 1], ['-', 'L', 'F'])) $neighbors[] = ['x' => $x - 1, 'y' => $y];
            if ($x < $cols - 1 && in_array($grid[$y][$x + 1], ['-', 'J', '7'])) $neighbors[] = ['x' => $x + 1, 'y' => $y];
            break;
    }
    return $neighbors;
}

solve();
?>
