
<?php

function solve() {
    $grid = array_map('trim', file("input.txt"));
    $h = count($grid);
    $w = strlen($grid[0]);

    $S = null;
    $E = null;
    $trackCells = [];
    $walls = [];
    for ($i = 0; $i < $h; $i++) {
        $walls[$i] = array_fill(0, $w, false);
    }

    for ($i = 0; $i < $h; $i++) {
        for ($j = 0; $j < $w; $j++) {
            $ch = $grid[$i][$j];
            if ($ch == 'S') {
                $S = ['x' => $i, 'y' => $j];
            } else if ($ch == 'E') {
                $E = ['x' => $i, 'y' => $j];
            }
            if ($ch == '#') {
                $walls[$i][$j] = true;
            } else {
                $trackCells[] = ['x' => $i, 'y' => $j];
            }
        }
    }

    $dirs = [['x' => 1, 'y' => 0], ['x' => -1, 'y' => 0], ['x' => 0, 'y' => 1], ['x' => 0, 'y' => -1]];

    $normalDistFrom = function ($start) use ($h, $w, $walls, $dirs) {
        $dist = [];
        for ($i = 0; $i < $h; $i++) {
            $dist[$i] = array_fill(0, $w, -1);
        }
        $dist[$start['x']][$start['y']] = 0;
        $q = [$start];
        while (!empty($q)) {
            $cur = array_shift($q);
            foreach ($dirs as $d) {
                $nx = $cur['x'] + $d['x'];
                $ny = $cur['y'] + $d['y'];
                if ($nx < 0 || $nx >= $h || $ny < 0 || $ny >= $w) {
                    continue;
                }
                if ($walls[$nx][$ny]) {
                    continue;
                }
                if ($dist[$nx][$ny] == -1) {
                    $dist[$nx][$ny] = $dist[$cur['x']][$cur['y']] + 1;
                    $q[] = ['x' => $nx, 'y' => $ny];
                }
            }
        }
        return $dist;
    };

    $distFromS = $normalDistFrom($S);
    $distFromE = $normalDistFrom($E);

    if ($distFromS[$E['x']][$E['y']] == -1) {
        echo 0 . "\n";
        return;
    }

    $normalCost = $distFromS[$E['x']][$E['y']];

    $isTrack = function ($x, $y) use ($h, $w, $walls) {
        if ($x < 0 || $x >= $h || $y < 0 || $y >= $w) {
            return false;
        }
        return !$walls[$x][$y];
    };

    $possibleCheats = 0;

    foreach ($trackCells as $startPos) {
        $sd = $distFromS[$startPos['x']][$startPos['y']];
        if ($sd == -1) {
            continue;
        }
        foreach ($dirs as $d1) {
            $m1x = $startPos['x'] + $d1['x'];
            $m1y = $startPos['y'] + $d1['y'];

            if ($m1x < 0 || $m1x >= $h || $m1y < 0 || $m1y >= $w) {
                continue;
            }
            foreach ($dirs as $d2) {
                $m2x = $m1x + $d2['x'];
                $m2y = $m1y + $d2['y'];
                if ($m2x < 0 || $m2x >= $h || $m2y < 0 || $m2y >= $w) {
                    continue;
                }
                if (!$isTrack($m2x, $m2y)) {
                    continue;
                }
                $ed = $distFromE[$m2x][$m2y];
                if ($ed == -1) {
                    continue;
                }
                $newCost = $sd + 2 + $ed;
                $saving = $normalCost - $newCost;
                if ($saving >= 100) {
                    $possibleCheats++;
                }
            }
        }
    }

    echo $possibleCheats . "\n";
}

solve();

?>
