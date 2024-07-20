
<?php

function readInput($filename) {
    return file($filename, FILE_IGNORE_NEW_LINES);
}

function isValidMove($x, $y, $grid, $visited) {
    return isset($grid[$y][$x]) && $grid[$y][$x] !== '#' && !isset($visited["$x,$y"]);
}

function dfs($x, $y, $grid, $visited, $direction, $steps) {
    // Mark the current tile as visited
    $visited["$x,$y"] = true;
    $maxSteps = $steps;

    // Possible moves: up, down, left, right
    $moves = [
        [-1, 0], // left
        [1, 0],  // right
        [0, -1], // up
        [0, 1]    // down
    ];

    // Check for slopes and their specific movements
    if (isset($grid[$y][$x]) && strpos('^>v<', $grid[$y][$x]) !== false) {
        switch ($grid[$y][$x]) {
            case '>':
                $nextX = $x + 1;
                $nextY = $y;
                break;
            case '<':
                $nextX = $x - 1;
                $nextY = $y;
                break;
            case 'v':
                $nextX = $x;
                $nextY = $y + 1;
                break;
            case '^':
                $nextX = $x;
                $nextY = $y - 1;
                break;
            default:
                return $maxSteps;
        }

        // Move in the direction of the slope
        if (isValidMove($nextX, $nextY, $grid, $visited)) {
            $maxSteps = max($maxSteps, dfs($nextX, $nextY, $grid, $visited, $grid[$nextY][$nextX], $steps + 1));
        }
    } else {
        // Explore all four directions
        foreach ($moves as [$dx, $dy]) {
            $nextX = $x + $dx;
            $nextY = $y + $dy;

            if (isValidMove($nextX, $nextY, $grid, $visited)) {
                $maxSteps = max($maxSteps, dfs($nextX, $nextY, $grid, $visited, null, $steps + 1));
            }
        }
    }

    // Unmark the current tile (backtrack)
    unset($visited["$x,$y"]);
    return $maxSteps;
}

function findLongestHike($grid) {
    $startX = strpos($grid[0], '.');
    $startY = 0;
    $visited = [];
    return dfs($startX, $startY, $grid, $visited, null, 0);
}

$inputFile = 'input.txt';
$grid = readInput($inputFile);
$longestHike = findLongestHike($grid);
echo "The longest hike is $longestHike steps long.\n";
