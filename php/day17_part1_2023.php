
<?php

function solve() {
    $grid = array_map('str_split', file('input.txt', FILE_IGNORE_NEW_LINES));
    $rows = count($grid);
    $cols = count($grid[0]);

    $start = [0, 0, 0, 0]; // row, col, direction (0: up, 1: right, 2: down, 3: left), steps in current direction
    $end = [$rows - 1, $cols - 1];

    $distances = [];
    $queue = new SplPriorityQueue();
    $queue->insert($start, 0);
    $distances[serialize($start)] = 0;

    $directions = [[-1, 0], [0, 1], [1, 0], [0, -1]];

    while (!$queue->isEmpty()) {
        $current = $queue->extract();
        $row = $current[0];
        $col = $current[1];
        $dir = $current[2];
        $steps = $current[3];
        $dist = $distances[serialize($current)];

        if ($row === $end[0] && $col === $end[1]) {
            echo $dist . PHP_EOL;
            return;
        }

        for ($newDir = 0; $newDir < 4; $newDir++) {
            $newSteps = ($newDir === $dir) ? $steps + 1 : 1;
            if ($newSteps > 3) continue;
            if (abs($newDir - $dir) === 2) continue; // Cannot reverse direction

            $newRow = $row + $directions[$newDir][0];
            $newCol = $col + $directions[$newDir][1];

            if ($newRow >= 0 && $newRow < $rows && $newCol >= 0 && $newCol < $cols) {
                $newDist = $dist + (int)$grid[$newRow][$newCol];
                $newState = [$newRow, $newCol, $newDir, $newSteps];
                $serializedState = serialize($newState);

                if (!isset($distances[$serializedState]) || $newDist < $distances[$serializedState]) {
                    $distances[$serializedState] = $newDist;
                    $queue->insert($newState, -$newDist);
                }
            }
        }
    }
}

solve();
?>
