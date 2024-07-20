
<?php

function readInput($filename) {
    return array_map('str_split', file($filename, FILE_IGNORE_NEW_LINES));
}

function expandMap($originalMap) {
    $originalHeight = count($originalMap);
    $originalWidth = count($originalMap[0]);
    $expandedMap = [];

    for ($i = 0; $i < $originalHeight * 5; $i++) {
        $expandedMap[$i] = [];
        for ($j = 0; $j < $originalWidth * 5; $j++) {
            $riskLevel = (intval($originalMap[$i % $originalHeight][$j % $originalWidth]) + intdiv($i, $originalHeight) + intdiv($j, $originalWidth) - 1) % 9 + 1;
            $expandedMap[$i][$j] = $riskLevel;
        }
    }

    return $expandedMap;
}

function dijkstra($map) {
    $height = count($map);
    $width = count($map[0]);
    $distances = array_fill(0, $height, array_fill(0, $width, PHP_INT_MAX));
    $distances[0][0] = 0;

    $priorityQueue = new SplPriorityQueue();
    $priorityQueue->insert([0, 0], 0);

    while (!$priorityQueue->isEmpty()) {
        [$x, $y] = $priorityQueue->extract();

        foreach ([[-1, 0], [1, 0], [0, -1], [0, 1]] as [$dx, $dy]) {
            $nx = $x + $dx;
            $ny = $y + $dy;

            if ($nx >= 0 && $nx < $height && $ny >= 0 && $ny < $width) {
                $newRisk = $distances[$x][$y] + $map[$nx][$ny];
                if ($newRisk < $distances[$nx][$ny]) {
                    $distances[$nx][$ny] = $newRisk;
                    $priorityQueue->insert([$nx, $ny], -$newRisk);
                }
            }
        }
    }

    return $distances[$height - 1][$width - 1];
}

$inputFile = 'input.txt';
$originalMap = readInput($inputFile);
$expandedMap = expandMap($originalMap);
$lowestRisk = dijkstra($expandedMap);

echo "The lowest total risk of any path from the top left to the bottom right is: $lowestRisk\n";

?>
