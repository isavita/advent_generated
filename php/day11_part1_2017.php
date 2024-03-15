<?php
$x = $y = $z = 0;
$maxDistance = 0;

$file = fopen("input.txt", "r");
$input = fgets($file);
fclose($file);

$directions = explode(",", trim($input));

foreach ($directions as $dir) {
    switch ($dir) {
        case "n":
            $y++;
            $z--;
            break;
        case "ne":
            $x++;
            $z--;
            break;
        case "se":
            $x++;
            $y--;
            break;
        case "s":
            $y--;
            $z++;
            break;
        case "sw":
            $x--;
            $z++;
            break;
        case "nw":
            $x--;
            $y++;
            break;
    }

    $curDistance = (abs($x) + abs($y) + abs($z)) / 2;
    $maxDistance = max($maxDistance, $curDistance);
}

echo (abs($x) + abs($y) + abs($z)) / 2 . PHP_EOL;
echo $maxDistance . PHP_EOL;