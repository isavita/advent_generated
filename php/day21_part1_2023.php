
<?php

function readInput($filename) {
    return file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
}

function countReachablePlots($map, $steps) {
    $rows = count($map);
    $cols = strlen($map[0]);
    $startX = $startY = -1;

    // Find the starting position 'S'
    for ($i = 0; $i < $rows; $i++) {
        for ($j = 0; $j < $cols; $j++) {
            if ($map[$i][$j] === 'S') {
                $startX = $i;
                $startY = $j;
                break 2;
            }
        }
    }

    // Directions for moving up, down, left, right
    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    $visited = [];
    $queue = [[$startX, $startY, 0]]; // (x, y, current_steps)
    $reachablePlots = [];

    while (!empty($queue)) {
        [$x, $y, $currentSteps] = array_shift($queue);

        // If we have reached the exact number of steps, record the position
        if ($currentSteps === $steps) {
            if (!isset($reachablePlots["$x,$y"])) {
                $reachablePlots["$x,$y"] = true;
            }
            continue;
        }

        // If we have exceeded the number of steps, skip this position
        if ($currentSteps > $steps) {
            continue;
        }

        // Explore the four possible directions
        foreach ($directions as [$dx, $dy]) {
            $newX = $x + $dx;
            $newY = $y + $dy;

            // Check if the new position is within bounds and is a garden plot
            if ($newX >= 0 && $newX < $rows && $newY >= 0 && $newY < $cols && 
                ($map[$newX][$newY] === '.' || $map[$newX][$newY] === 'S') && 
                !isset($visited["$newX,$newY,$currentSteps"])) {
                
                $visited["$newX,$newY,$currentSteps"] = true;
                $queue[] = [$newX, $newY, $currentSteps + 1];
            }
        }
    }

    return count($reachablePlots);
}

// Main program
$inputFile = 'input.txt';
$map = readInput($inputFile);
$steps = 64;
$result = countReachablePlots($map, $steps);
echo "Number of reachable garden plots in exactly $steps steps: $result\n";
?>
