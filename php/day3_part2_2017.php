<?php

$data = file_get_contents("input.txt");
$target = intval(trim($data));

$grid = [];
$grid["0,0"] = 1; // Using a string key "x,y"

$x = $y = 0;
$dx = 0;
$dy = -1;

while (true) {
    // Change direction when reaching a corner
    if ($x == $y || ($x < 0 && $x == -$y) || ($x > 0 && $x == 1 - $y)) {
        $temp = $dx;
        $dx = -$dy;
        $dy = $temp;
    }

    // Move to the next square
    $x += $dx;
    $y += $dy;

    // Calculate value for the current square
    $value = 0;
    for ($i = -1; $i <= 1; $i++) {
        for ($j = -1; $j <= 1; $j++) {
            $key = ($x + $i) . "," . ($y + $j); // Create the key for neighboring cells
            $value += $grid[$key] ?? 0; // Use null coalescing operator to handle undefined keys
        }
    }
    $grid["$x,$y"] = $value; // Store the computed value in the grid

    // Check if value is greater than the target
    if ($value > $target) {
        echo $value;
        break;
    }
}

?>