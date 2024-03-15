<?php

function myAbs($x) {
    return $x < 0 ? -$x : $x;
}

function sign($x) {
    return $x > 0 ? 1 : ($x < 0 ? -1 : 0);
}

$file = fopen("input.txt", "r");
if (!$file) {
    die("Failed to open input file.");
}

$lines = [];
while (($line = fgets($file)) !== false) {
    $parts = explode(" -> ", trim($line));
    $start = explode(",", $parts[0]);
    $end = explode(",", $parts[1]);

    $x1 = (int)$start[0];
    $y1 = (int)$start[1];
    $x2 = (int)$end[0];
    $y2 = (int)$end[1];

    $lines[] = [$x1, $y1, $x2, $y2];
}
fclose($file);

$overlaps = [];
foreach ($lines as $line) {
    list($x1, $y1, $x2, $y2) = $line;

    $xStep = sign($x2 - $x1);
    $yStep = sign($y2 - $y1);
    $steps = max(myAbs($x2 - $x1), myAbs($y2 - $y1)) + 1;

    for ($i = 0; $i < $steps; $i++) {
        $point = [$x1 + $i * $xStep, $y1 + $i * $yStep];
        $overlaps[$point[0] . "," . $point[1]] = ($overlaps[$point[0] . "," . $point[1]] ?? 0) + 1;
    }
}

$count = 0;
foreach ($overlaps as $v) {
    if ($v > 1) {
        $count++;
    }
}

echo $count . "\n";