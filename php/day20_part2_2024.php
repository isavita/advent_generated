
<?php

$grid = array();
foreach (file('input.txt') as $line) {
    $line = trim($line);
    $grid[] = str_split($line);
}

$S = null;
$E = null;
foreach ($grid as $x => $row) {
    foreach ($row as $y => $cell) {
        if ($cell == 'S') {
            $S = array($x, $y);
        } elseif ($cell == 'E') {
            $E = array($x, $y);
        }
    }
}

function computeDistances($grid, $start) {
    $rows = count($grid);
    if ($rows == 0) return array();
    $cols = count($grid[0]);
    $distance = array_fill(0, $rows, array_fill(0, $cols, -1));
    $queue = new SplQueue();
    $queue->enqueue($start);
    $x_start = $start[0];
    $y_start = $start[1];
    $distance[$x_start][$y_start] = 0;

    $dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while (!$queue->isEmpty()) {
        $current = $queue->dequeue();
        $x = $current[0];
        $y = $current[1];
        foreach ($dirs as $d) {
            $nx = $x + $d[0];
            $ny = $y + $d[1];
            if ($nx >= 0 && $nx < $rows && $ny >= 0 && $ny < count($grid[$nx])) {
                if ($grid[$nx][$ny] != '#' && $distance[$nx][$ny] == -1) {
                    $distance[$nx][$ny] = $distance[$x][$y] + 1;
                    $queue->enqueue(array($nx, $ny));
                }
            }
        }
    }

    return $distance;
}

$A = computeDistances($grid, $S);
$B = computeDistances($grid, $E);

$track = array();
foreach ($grid as $x => $row) {
    foreach ($row as $y => $cell) {
        if ($cell == '.' || $cell == 'S' || $cell == 'E') {
            $track[] = array($x, $y);
        }
    }
}

$D = $A[$E[0]][$E[1]];
$count = 0;

foreach ($track as $s) {
    $s_x = $s[0];
    $s_y = $s[1];
    $a_s = $A[$s_x][$s_y];
    if ($a_s == -1) {
        continue;
    }
    foreach ($track as $e) {
        $e_x = $e[0];
        $e_y = $e[1];
        if ($s_x == $e_x && $s_y == $e_y) {
            continue;
        }
        $b_e = $B[$e_x][$e_y];
        if ($b_e == -1) {
            continue;
        }
        $k_min = abs($s_x - $e_x) + abs($s_y - $e_y);
        if ($k_min > 20) {
            continue;
        }
        $new_time = $a_s + $k_min + $b_e;
        $saving = $D - $new_time;
        if ($saving >= 100) {
            $count++;
        }
    }
}

echo $count;
