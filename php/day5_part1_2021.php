
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$grid = [];

foreach ($lines as $line) {
    $coords = explode(" -> ", $line);
    $startCoords = explode(",", $coords[0]);
    $endCoords = explode(",", $coords[1]);

    $x1 = (int)$startCoords[0];
    $y1 = (int)$startCoords[1];
    $x2 = (int)$endCoords[0];
    $y2 = (int)$endCoords[1];

    if ($x1 == $x2) {
        if ($y1 > $y2) {
            list($y1, $y2) = [$y2, $y1];
        }
        for ($y = $y1; $y <= $y2; $y++) {
            $grid["$x1,$y"] = isset($grid["$x1,$y"]) ? $grid["$x1,$y"] + 1 : 1;
        }
    } elseif ($y1 == $y2) {
        if ($x1 > $x2) {
            list($x1, $x2) = [$x2, $x1];
        }
        for ($x = $x1; $x <= $x2; $x++) {
            $grid["$x,$y1"] = isset($grid["$x,$y1"]) ? $grid["$x,$y1"] + 1 : 1;
        }
    }
}

$overlapCount = 0;
foreach ($grid as $v) {
    if ($v > 1) {
        $overlapCount++;
    }
}

echo $overlapCount . PHP_EOL;
?>
