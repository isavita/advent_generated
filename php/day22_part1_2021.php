
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$rebootSteps = [];

foreach ($lines as $line) {
    if ($line == "") {
        continue;
    }
    $step = parseRebootStep($line);
    $rebootSteps[] = $step;
}

$minCoord = -50;
$maxCoord = 50;
$cubeGrid = createCubeGrid($minCoord, $maxCoord);
executeRebootSteps($cubeGrid, $rebootSteps);
$onCubes = countOnCubes($cubeGrid);

echo $onCubes . "\n";

function parseRebootStep($line) {
    $parts = explode(" ", $line);

    $action = $parts[0];
    $parts = explode(",", $parts[1]);
    $xRange = explode("..", substr($parts[0], 2));
    $yRange = explode("..", substr($parts[1], 2));
    $zRange = explode("..", substr($parts[2], 2));

    $xStart = (int)$xRange[0];
    $xEnd = (int)$xRange[1];
    $yStart = (int)$yRange[0];
    $yEnd = (int)$yRange[1];
    $zStart = (int)$zRange[0];
    $zEnd = (int)$zRange[1];

    return [
        "action" => $action,
        "xStart" => $xStart,
        "xEnd" => $xEnd,
        "yStart" => $yStart,
        "yEnd" => $yEnd,
        "zStart" => $zStart,
        "zEnd" => $zEnd
    ];
}

function createCubeGrid($minCoord, $maxCoord) {
    $gridSize = $maxCoord - $minCoord + 1;
    $grid = [];

    for ($i = 0; $i < $gridSize; $i++) {
        $grid[$i] = [];

        for ($j = 0; $j < $gridSize; $j++) {
            $grid[$i][$j] = array_fill(0, $gridSize, false);
        }
    }

    return $grid;
}

function executeRebootSteps(&$cubeGrid, $rebootSteps) {
    foreach ($rebootSteps as $step) {
        if (!($step["xStart"] >= -50 && $step["xEnd"] <= 50 && $step["yStart"] >= -50 && $step["yEnd"] <= 50 && $step["zStart"] >= -50 && $step["zEnd"] <= 50)) {
            continue;
        }

        for ($x = $step["xStart"]; $x <= $step["xEnd"]; $x++) {
            for ($y = $step["yStart"]; $y <= $step["yEnd"]; $y++) {
                for ($z = $step["zStart"]; $z <= $step["zEnd"]; $z++) {
                    $cubeGrid[$x + 50][$y + 50][$z + 50] = $step["action"] == "on";
                }
            }
        }
    }
}

function countOnCubes($cubeGrid) {
    $count = 0;

    foreach ($cubeGrid as $i => $row) {
        foreach ($row as $j => $col) {
            foreach ($col as $k => $cube) {
                if ($cube) {
                    $count++;
                }
            }
        }
    }

    return $count;
}
