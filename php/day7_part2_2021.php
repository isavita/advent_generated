<?php
$file = fopen("input.txt", "r");
$positions = [];
while (($line = fgets($file)) !== false) {
    $numbers = explode(",", trim($line));
    foreach ($numbers as $num_str) {
        $positions[] = (int)$num_str;
    }
}
fclose($file);

sort($positions);

$min_fuel = PHP_INT_MAX;
for ($i = $positions[0]; $i <= $positions[count($positions) - 1]; $i++) {
    $fuel = 0;
    foreach ($positions as $pos) {
        $fuel += calculateNewFuel($pos, $i);
    }
    if ($fuel < $min_fuel) {
        $min_fuel = $fuel;
    }
}
echo $min_fuel;

function calculateNewFuel($currentPosition, $newPosition)
{
    $diff = abs($currentPosition - $newPosition);
    return ($diff * ($diff + 1)) / 2;
}