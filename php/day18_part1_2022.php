
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$cubes = [];

foreach ($lines as $line) {
    $coords = explode(",", $line);
    $x = (int)$coords[0];
    $y = (int)$coords[1];
    $z = (int)$coords[2];
    $cubes["$x,$y,$z"] = true;
}

$surfaceArea = 0;
$directions = [
    [1, 0, 0], [-1, 0, 0], // x directions
    [0, 1, 0], [0, -1, 0], // y directions
    [0, 0, 1], [0, 0, -1], // z directions
];

foreach ($cubes as $cube => $value) {
    $point = explode(",", $cube);
    $x = (int)$point[0];
    $y = (int)$point[1];
    $z = (int)$point[2];
    $exposedSides = 6;

    foreach ($directions as $dir) {
        $adjacent = ($x + $dir[0]) . "," . ($y + $dir[1]) . "," . ($z + $dir[2]);
        if (isset($cubes[$adjacent])) {
            $exposedSides--;
        }
    }

    $surfaceArea += $exposedSides;
}

echo $surfaceArea . "\n";
?>
