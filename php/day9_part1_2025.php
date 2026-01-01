<?php
$lines = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$x = [];
$y = [];
foreach ($lines as $line) {
    $line = trim($line);
    if ($line === '') continue;
    $parts = explode(',', $line, 2);
    if (count($parts) != 2) continue;
    $xi = (int)trim($parts[0]);
    $yi = (int)trim($parts[1]);
    $x[] = $xi;
    $y[] = $yi;
}
$n = count($x);
if ($n === 0) { exit; }
$maxArea = 1;
for ($i = 0; $i < $n; $i++) {
    $x1 = $x[$i];
    $y1 = $y[$i];
    for ($j = $i + 1; $j < $n; $j++) {
        $width = abs($x1 - $x[$j]) + 1;
        $height = abs($y1 - $y[$j]) + 1;
        $area = $width * $height;
        if ($area > $maxArea) $maxArea = $area;
    }
}
echo $maxArea . PHP_EOL;
?>