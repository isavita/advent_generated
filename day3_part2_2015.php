
<?php

$data = file_get_contents("input.txt");
$directions = str_split($data);
$visitedHouses = [];
$xSanta = 0;
$ySanta = 0;
$xRobo = 0;
$yRobo = 0;
$isSantaTurn = true;

$visitedHouses["$xSanta,$ySanta"] = true;

foreach ($directions as $dir) {
    if ($isSantaTurn) {
        $x = &$xSanta;
        $y = &$ySanta;
    } else {
        $x = &$xRobo;
        $y = &$yRobo;
    }

    switch ($dir) {
        case '^':
            $y++; // Move north
            break;
        case 'v':
            $y--; // Move south
            break;
        case '>':
            $x++; // Move east
            break;
        case '<':
            $x--; // Move west
            break;
    }

    $visitedHouses["$x,$y"] = true;
    $isSantaTurn = !$isSantaTurn;
}

echo count($visitedHouses) . PHP_EOL;
?>
