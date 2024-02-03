
<?php

$gridSize = 1000;
$grid = array_fill(0, $gridSize, array_fill(0, $gridSize, 0));

$file = fopen("input.txt", "r");
if ($file) {
    while (($instruction = fgets($file)) !== false) {
        processInstruction(trim($instruction), $grid);
    }
    fclose($file);
}

echo totalBrightness($grid) . PHP_EOL;

function processInstruction($instruction, &$grid) {
    $parts = explode(" ", $instruction);
    $start = explode(",", $parts[count($parts) - 3]);
    $end = explode(",", $parts[count($parts) - 1]);
    $startX = (int)$start[0];
    $startY = (int)$start[1];
    $endX = (int)$end[0];
    $endY = (int)$end[1];

    for ($x = $startX; $x <= $endX; $x++) {
        for ($y = $startY; $y <= $endY; $y++) {
            switch (true) {
                case strpos($instruction, "turn on") === 0:
                    $grid[$x][$y]++;
                    break;
                case strpos($instruction, "turn off") === 0:
                    if ($grid[$x][$y] > 0) {
                        $grid[$x][$y]--;
                    }
                    break;
                case strpos($instruction, "toggle") === 0:
                    $grid[$x][$y] += 2;
                    break;
            }
        }
    }
}

function totalBrightness($grid) {
    $brightness = 0;
    foreach ($grid as $row) {
        foreach ($row as $light) {
            $brightness += $light;
        }
    }
    return $brightness;
}
?>
