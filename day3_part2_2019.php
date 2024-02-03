
<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$wire1 = getPointsWithSteps($lines[0]);
$wire2 = getPointsWithSteps($lines[1]);

$minSteps = PHP_INT_MAX;
foreach ($wire1 as $p => $steps1) {
    if (isset($wire2[$p])) {
        $steps2 = $wire2[$p];
        $totalSteps = $steps1 + $steps2;
        if ($totalSteps < $minSteps) {
            $minSteps = $totalSteps;
        }
    }
}

echo $minSteps . "\n";

function getPointsWithSteps($path) {
    $points = [];
    $current = ['X' => 0, 'Y' => 0];
    $steps = 0;
    $moves = explode(",", $path);
    foreach ($moves as $move) {
        $dir = $move[0];
        $dist = (int) substr($move, 1);
        for ($i = 0; $i < $dist; $i++) {
            $steps++;
            switch ($dir) {
                case 'U':
                    $current['Y']++;
                    break;
                case 'D':
                    $current['Y']--;
                    break;
                case 'L':
                    $current['X']--;
                    break;
                case 'R':
                    $current['X']++;
                    break;
            }
            $point = json_encode($current);
            if (!array_key_exists($point, $points)) {
                $points[$point] = $steps;
            }
        }
    }
    return $points;
}
?>
