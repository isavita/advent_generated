
<?php

$data = file_get_contents("input.txt");
$directions = str_split($data);
$visitedHouses = [];
$x = 0;
$y = 0;

$visitedHouses["$x,$y"] = true;

foreach ($directions as $dir) {
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
}

echo count($visitedHouses) . PHP_EOL;
?>
