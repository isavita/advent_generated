
<?php

function readInput($filename) {
    return file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
}

function findStartAndEnd($grid) {
    $start = $end = null;
    foreach ($grid as $row => $line) {
        if (($pos = strpos($line, 'S')) !== false) {
            $start = [$row, $pos];
            $grid[$row][$pos] = 'a'; // Treat 'S' as 'a'
        }
        if (($pos = strpos($line, 'E')) !== false) {
            $end = [$row, $pos];
            $grid[$row][$pos] = 'z'; // Treat 'E' as 'z'
        }
    }
    return [$start, $end, $grid];
}

function isValidMove($currentHeight, $nextHeight) {
    return $nextHeight <= $currentHeight + 1;
}

function bfs($grid, $start, $end) {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $queue = [[$start[0], $start[1], 0]]; // row, col, steps
    $visited = [];

    while (!empty($queue)) {
        [$row, $col, $steps] = array_shift($queue);
        
        if (isset($visited["$row,$col"])) {
            continue;
        }
        $visited["$row,$col"] = true;

        if ($row === $end[0] && $col === $end[1]) {
            return $steps;
        }

        $directions = [[0, 1], [1, 0], [0, -1], [-1, 0]]; // right, down, left, up
        foreach ($directions as [$dr, $dc]) {
            $newRow = $row + $dr;
            $newCol = $col + $dc;

            if ($newRow >= 0 && $newRow < $rows && $newCol >= 0 && $newCol < $cols) {
                $currentHeight = ord($grid[$row][$col]);
                $nextHeight = ord($grid[$newRow][$newCol]);
                
                if (isValidMove($currentHeight, $nextHeight)) {
                    $queue[] = [$newRow, $newCol, $steps + 1];
                }
            }
        }
    }

    return -1; // If no path found
}

function main() {
    $grid = readInput('input.txt');
    list($start, $end, $grid) = findStartAndEnd($grid);
    $steps = bfs($grid, $start, $end);
    echo "Fewest steps required: $steps\n";
}

main();
