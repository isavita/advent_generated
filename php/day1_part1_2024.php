
<?php

$input = file('input.txt', FILE_IGNORE_NEW_LINES);
$left = [];
$right = [];

foreach ($input as $line) {
    list($l, $r) = explode('   ', $line);
    $left[] = (int)$l;
    $right[] = (int)$r;
}

sort($left);
sort($right);

$totalDistance = 0;
for ($i = 0; $i < count($left); $i++) {
    $totalDistance += abs($left[$i] - $right[$i]);
}

echo "Total distance: " . $totalDistance . PHP_EOL;

?>
