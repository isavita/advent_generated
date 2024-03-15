<?php
$file = fopen("input.txt", "r");
$total = 0;
while (($line = fgets($file)) !== false) {
    $dimensions = explode("x", trim($line));
    if (count($dimensions) != 3) {
        die("Invalid input format");
    }
    $l = intval($dimensions[0]);
    $w = intval($dimensions[1]);
    $h = intval($dimensions[2]);
    $side1 = $l * $w;
    $side2 = $w * $h;
    $side3 = $h * $l;
    $smallest = min($side1, $side2, $side3);
    $total += 2 * $side1 + 2 * $side2 + 2 * $side3 + $smallest;
}
fclose($file);
echo $total;