<?php
$file = fopen("input.txt", "r");
$points = [];
$maxX = $maxY = 0;
while (($line = fgets($file)) !== false) {
    $coords = explode(", ", trim($line));
    $x = (int)$coords[0];
    $y = (int)$coords[1];
    if ($x > $maxX) {
        $maxX = $x;
    }
    if ($y > $maxY) {
        $maxY = $y;
    }
    $points[] = ["x" => $x, "y" => $y];
}
fclose($file);

$grid = array_fill(0, $maxX + 2, array_fill(0, $maxY + 2, 0));
$areas = array_fill(0, count($points), 0);
$infinite = array_fill(0, count($points), false);
for ($i = 0; $i <= $maxX + 1; $i++) {
    for ($j = 0; $j <= $maxY + 1; $j++) {
        $minDist = $maxX + $maxY;
        for ($k = 0; $k < count($points); $k++) {
            $dist = abs($points[$k]["x"] - $i) + abs($points[$k]["y"] - $j);
            if ($dist < $minDist) {
                $minDist = $dist;
                $grid[$i][$j] = $k;
            } elseif ($dist == $minDist) {
                $grid[$i][$j] = -1;
            }
        }
        if ($grid[$i][$j] != -1) {
            if ($i == 0 || $j == 0 || $i == $maxX + 1 || $j == $maxY + 1) {
                $infinite[$grid[$i][$j]] = true;
            }
            $areas[$grid[$i][$j]]++;
        }
    }
}

$maxArea = 0;
for ($i = 0; $i < count($areas); $i++) {
    if (!$infinite[$i] && $areas[$i] > $maxArea) {
        $maxArea = $areas[$i];
    }
}
echo $maxArea . "\n";