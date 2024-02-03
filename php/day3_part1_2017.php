
<?php

$input = file_get_contents("input.txt");
$target = intval(trim($input));

$sideLength = ceil(sqrt($target));
if ($sideLength % 2 == 0) {
    $sideLength++;
}

$maxValue = $sideLength * $sideLength;
$stepsFromEdge = ($sideLength - 1) / 2;
$distanceToMiddle = 0;

for ($i = 0; $i < 4; $i++) {
    $middlePoint = $maxValue - $stepsFromEdge - ($sideLength - 1) * $i;
    $distance = abs($target - $middlePoint);
    if ($distance < $distanceToMiddle || $i == 0) {
        $distanceToMiddle = $distance;
    }
}

$manhattanDistance = $stepsFromEdge + $distanceToMiddle;

echo $manhattanDistance . PHP_EOL;
?>
