
<?php

function solve() {
    $input = file_get_contents("input.txt");
    $lines = explode("\n", trim($input));
    $grid = [];
    foreach ($lines as $y => $line) {
        for ($x = 0; $x < strlen($line); $x++) {
            if ($line[$x] != '.') {
                $grid[$x . "," . $y] = $line[$x];
            }
        }
    }

    $bounds = getBounds(array_keys($grid));
    $entrance = "1,0";
    $exit = ($bounds[2] - 2) . "," . ($bounds[3] - 1);
    $crossing = steps($grid, $bounds, $entrance, $exit, 0);

    echo $crossing . "\n";
}

function steps(array $grid, array $bounds, string $start, string $end, int $initialStep): int {
    $q = [[$start, $initialStep]];
    $seen = [];
    $neighbors4 = [[0, 1], [0, -1], [1, 0], [-1, 0]];

    while (!empty($q)) {
        $curr = array_shift($q);
        $currPos = $curr[0];
        $currStep = $curr[1];

        if ($currPos === $end) {
            return $currStep;
        }

        for ($i = -1; $i < 5; $i++) {
            if ($i == 4) {
                $n = [0,0];
            } else {
                $n = $neighbors4[$i];
            }
            
            list($x,$y) = explode(",", $currPos);
            $nextX = (int)$x + $n[0];
            $nextY = (int)$y + $n[1];
            $nextPos = $nextX . "," . $nextY;
            $nextStep = $currStep + 1;
            $nextState = $nextPos . "," . $nextStep;

            if (isset($seen[$nextState])) {
                continue;
            }

            if ($nextX < $bounds[0] || $nextX >= $bounds[2] || $nextY < $bounds[1] || $nextY >= $bounds[3]) {
                continue;
            }

            if (isset($grid[$nextPos]) && $grid[$nextPos] == '#') {
                continue;
            }

            if ($nextY > 0 && $nextY < $bounds[3] - 1) {
                $blizzards = ['^', '>', 'v', '<'];
                foreach ($blizzards as $bliz) {
                    $prevPos = getPrevPos($nextX, $nextY, $nextStep, $bliz, $bounds);
                    if (isset($grid[$prevPos]) && $grid[$prevPos] == $bliz) {
                        continue 2;
                    }
                }
            }

            $q[] = [$nextPos, $nextStep];
            $seen[$nextState] = true;
        }
    }
    return -1;
}

function getPrevPos(int $x, int $y, int $step, string $bliz, array $bounds): string {
    $dir = dirFromByte($bliz);
    $dx = $dir[0];
    $dy = $dir[1];
    $prevX = ($x - ($dx * $step));
    $prevY = ($y - ($dy * $step));
    
    $width = $bounds[2] - $bounds[0] - 2;
    $height = $bounds[3] - $bounds[1] - 2;
    
    $prevX = (($prevX - 1) % $width);
    if ($prevX < 0) {
        $prevX += $width;
    }
    $prevX += 1;
    
    $prevY = (($prevY - 1) % $height);
    if ($prevY < 0) {
        $prevY += $height;
    }
    $prevY += 1;
    
    return $prevX . "," . $prevY;
}

function getBounds(array $keys): array {
    $minX = PHP_INT_MAX;
    $minY = PHP_INT_MAX;
    $maxX = PHP_INT_MIN;
    $maxY = PHP_INT_MIN;

    foreach ($keys as $key) {
        list($x, $y) = explode(",", $key);
        $x = (int)$x;
        $y = (int)$y;
        $minX = min($minX, $x);
        $minY = min($minY, $y);
        $maxX = max($maxX, $x);
        $maxY = max($maxY, $y);
    }

    return [$minX, $minY, $maxX + 1, $maxY + 1];
}

function dirFromByte(string $b): array {
    $fromByte = [
        '^' => [0, -1],
        '>' => [1, 0],
        'v' => [0, 1],
        '<' => [-1, 0],
    ];
    return $fromByte[$b];
}

solve();
?>
