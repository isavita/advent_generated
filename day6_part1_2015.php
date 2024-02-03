
<?php

$gridSize = 1000;
$grid = array_fill(0, $gridSize, array_fill(0, $gridSize, false));

$file = fopen("input.txt", "r");
if ($file) {
    while (($instruction = fgets($file)) !== false) {
        processInstruction(trim($instruction), $grid);
    }
    fclose($file);
}

echo countLights($grid) . PHP_EOL;

function processInstruction($instruction, &$grid) {
    $parts = explode(" ", $instruction);
    sscanf($parts[count($parts) - 3], "%d,%d", $startX, $startY);
    sscanf($parts[count($parts) - 1], "%d,%d", $endX, $endY);

    for ($x = $startX; $x <= $endX; $x++) {
        for ($y = $startY; $y <= $endY; $y++) {
            switch (true) {
                case strpos($instruction, "turn on") === 0:
                    $grid[$x][$y] = true;
                    break;
                case strpos($instruction, "turn off") === 0:
                    $grid[$x][$y] = false;
                    break;
                case strpos($instruction, "toggle") === 0:
                    $grid[$x][$y] = !$grid[$x][$y];
                    break;
            }
        }
    }
}

function countLights($grid) {
    $count = 0;
    foreach ($grid as $row) {
        foreach ($row as $light) {
            if ($light) {
                $count++;
            }
        }
    }
    return $count;
}
?>
