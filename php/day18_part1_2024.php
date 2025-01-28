
<?php

function solve() {
    $byte_positions = file('input.txt', FILE_IGNORE_NEW_LINES);
    if ($byte_positions === false) {
        die("Failed to read input.txt\n");
    }

    $grid_size = 71; // Grid is 0-70, so size is 71
    $grid = array_fill(0, $grid_size, array_fill(0, $grid_size, 0)); // 0: safe, 1: corrupted

    // Simulate first kilobyte (1024 bytes) falling
    $bytes_to_simulate = min(1024, count($byte_positions));
    for ($i = 0; $i < $bytes_to_simulate; ++$i) {
        list($x, $y) = explode(',', $byte_positions[$i]);
        $x = intval($x);
        $y = intval($y);
        if ($x >= 0 && $x < $grid_size && $y >= 0 && $y < $grid_size) {
            $grid[$y][$x] = 1; // Mark as corrupted
        }
    }

    $start_x = 0;
    $start_y = 0;
    $end_x = 70;
    $end_y = 70;

    if ($grid[$start_y][$start_x] == 1 || $grid[$end_y][$end_x] == 1) {
        return -1; // Start or end is corrupted, no path possible
    }

    $queue = [[$start_x, $start_y, 0]]; // [x, y, steps]
    $visited = array_fill(0, $grid_size, array_fill(0, $grid_size, false));
    $visited[$start_y][$start_x] = true;

    $directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]; // Down, Up, Right, Left

    while (!empty($queue)) {
        list($current_x, $current_y, $steps) = array_shift($queue);

        if ($current_x == $end_x && $current_y == $end_y) {
            return $steps;
        }

        foreach ($directions as $dir) {
            $next_x = $current_x + $dir[0];
            $next_y = $current_y + $dir[1];

            if ($next_x >= 0 && $next_x < $grid_size && $next_y >= 0 && $next_y < $grid_size &&
                $grid[$next_y][$next_x] == 0 && !$visited[$next_y][$next_x]) {
                $visited[$next_y][$next_x] = true;
                $queue[] = [$next_x, $next_y, $steps + 1];
            }
        }
    }

    return -1; // No path found
}

$shortest_path = solve();
echo $shortest_path . "\n";

?>
