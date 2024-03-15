<?php
$data = file_get_contents("input.txt");
$serial = intval(trim($data));

$gridSize = 300;
$grid = array_fill(0, $gridSize, array_fill(0, $gridSize, 0));

for ($y = 0; $y < $gridSize; $y++) {
    for ($x = 0; $x < $gridSize; $x++) {
        $rackID = $x + 11;
        $powerLevel = $rackID * ($y + 1);
        $powerLevel += $serial;
        $powerLevel *= $rackID;
        $powerLevel = (int)($powerLevel / 100) % 10;
        $powerLevel -= 5;
        $grid[$y][$x] = $powerLevel;
    }
}

$maxPower = PHP_INT_MIN;
$maxX = $maxY = 0;
for ($y = 0; $y < $gridSize - 2; $y++) {
    for ($x = 0; $x < $gridSize - 2; $x++) {
        $totalPower = 0;
        for ($dy = 0; $dy < 3; $dy++) {
            for ($dx = 0; $dx < 3; $dx++) {
                $totalPower += $grid[$y + $dy][$x + $dx];
            }
        }
        if ($totalPower > $maxPower) {
            $maxPower = $totalPower;
            $maxX = $x + 1;
            $maxY = $y + 1;
        }
    }
}

echo "$maxX,$maxY\n";
?>