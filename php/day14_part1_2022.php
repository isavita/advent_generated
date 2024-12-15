
<?php

function solve() {
    $grid = [];
    $file = fopen("input.txt", "r");
    if ($file) {
        while (($line = fgets($file)) !== false) {
            $parts = explode(" -> ", trim($line));
            $points = [];
            foreach ($parts as $part) {
                sscanf($part, "%d,%d", $x, $y);
                $points[] = ['x' => $x, 'y' => $y];
            }
            for ($i = 0; $i < count($points) - 1; $i++) {
                $p1 = $points[$i];
                $p2 = $points[$i + 1];
                if ($p1['x'] == $p2['x']) {
                    for ($y = min($p1['y'], $p2['y']); $y <= max($p1['y'], $p2['y']); $y++) {
                        $grid[$p1['x'] . "," . $y] = true;
                    }
                } else {
                    for ($x = min($p1['x'], $p2['x']); $x <= max($p1['x'], $p2['x']); $x++) {
                        $grid[$x . "," . $p1['y']] = true;
                    }
                }
            }
        }
        fclose($file);
    }
    echo fill($grid) . PHP_EOL;
}

function fill(&$grid) {
    $floor = bounds($grid)['maxY'] + 1;
    $sands = 0;
    $firstFloorTouch = 0;
    while (!isset($grid["500,0"])) {
        $sand = ['x' => 500, 'y' => 0];
        $settled = false;
        while (!$settled) {
            $next = next_sand($grid, $sand, $floor);
            if ($next === null) {
                $settled = true;
            } else {
                $sand = $next;
                if ($sand['y'] == $floor) {
                    if ($firstFloorTouch == 0) {
                        $firstFloorTouch = $sands;
                    }
                    $grid[$sand['x'] . "," . $sand['y']] = true;
                    $settled = true;
                }
            }
        }
        $sands++;
    }
    return $firstFloorTouch;
}

function next_sand(&$grid, $sand, $floor) {
    $down = ['x' => $sand['x'], 'y' => $sand['y'] + 1];
    $downLeft = ['x' => $sand['x'] - 1, 'y' => $sand['y'] + 1];
    $downRight = ['x' => $sand['x'] + 1, 'y' => $sand['y'] + 1];

    foreach ([$down, $downLeft, $downRight] as $next) {
        if (!isset($grid[$next['x'] . "," . $next['y']])) {
            return $next;
        }
    }
    $grid[$sand['x'] . "," . $sand['y']] = true;
    return null;
}

function bounds($grid) {
    $minX = PHP_INT_MAX;
    $maxX = PHP_INT_MIN;
    $minY = PHP_INT_MAX;
    $maxY = PHP_INT_MIN;

    foreach ($grid as $key => $value) {
        list($x, $y) = explode(",", $key);
        $x = (int)$x;
        $y = (int)$y;
        $minX = min($minX, $x);
        $maxX = max($maxX, $x);
        $minY = min($minY, $y);
        $maxY = max($maxY, $y);
    }
    return ['minX' => $minX, 'maxX' => $maxX, 'minY' => $minY, 'maxY' => $maxY];
}

solve();
?>
