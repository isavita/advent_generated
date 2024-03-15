<?php
function myAbs($x) {
    return $x < 0 ? -$x : $x;
}

function myMax($a, $b) {
    return $a > $b ? $a : $b;
}

function distance($x, $y, $z) {
    return (myAbs($x) + myAbs($y) + myAbs($z)) / 2;
}

$file = fopen("input.txt", "r");
$input = fgets($file);
fclose($file);

$directions = explode(",", trim($input));

$x = $y = $z = 0;
$maxDistance = 0;

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

    $curDistance = distance($x, $y, $z);
    $maxDistance = myMax($maxDistance, $curDistance);
}

echo $maxDistance;
?>