<?php

$data = file_get_contents("input.txt");
$serial = intval(trim($data));

$gridSize = 300;
$grid = array_fill(0, $gridSize, array_fill(0, $gridSize, 0));
$summedAreaTable = array_fill(0, $gridSize + 1, array_fill(0, $gridSize + 1, 0));

// Calculate power level for each cell
for ($y = 1; $y <= $gridSize; $y++) {
    for ($x = 1; $x <= $gridSize; $x++) {
        $rackID = $x + 10;
        $powerLevel = $rackID * $y;
        $powerLevel += $serial;
        $powerLevel *= $rackID;
        $powerLevel = (int)($powerLevel / 100) % 10;
        $powerLevel -= 5;
        $grid[$y - 1][$x - 1] = $powerLevel;

        // Build the summed-area table
        $summedAreaTable[$y][$x] = $powerLevel
            + $summedAreaTable[$y - 1][$x]
            + $summedAreaTable[$y][$x - 1]
            - $summedAreaTable[$y - 1][$x - 1];
    }
}

$maxPower = PHP_INT_MIN;
$maxX = $maxY = $maxSize = 0;

// Calculate maximum power for all possible square sizes
for ($size = 1; $size <= $gridSize; $size++) {
    for ($y = 1; $y <= $gridSize - $size + 1; $y++) {
        for ($x = 1; $x <= $gridSize - $size + 1; $x++) {
            $totalPower = $summedAreaTable[$y + $size - 1][$x + $size - 1]
                - $summedAreaTable[$y - 1][$x + $size - 1]
                - $summedAreaTable[$y + $size - 1][$x - 1]
                + $summedAreaTable[$y - 1][$x - 1];

            if ($totalPower > $maxPower) {
                $maxPower = $totalPower;
                $maxX = $x;
                $maxY = $y;
                $maxSize = $size;
            }
        }
    }
}

echo "$maxX,$maxY,$maxSize\n";

?>