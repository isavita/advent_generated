<?php

function cleaningRobot($input) {
    $grid = [];
    foreach (explode("\n", $input) as $line) {
        $grid[] = str_split($line);
    }

    $graph = [];
    foreach ($grid as $r => $row) {
        foreach ($row as $c => $cell) {
            if (preg_match('/[0-9]/', $cell)) {
                $poi = $cell;
                $distancesFromPOI = bfsGetEdgeWeights($grid, [$r, $c]);

                if (empty($graph)) {
                    $graph = array_fill(0, count($distancesFromPOI), array_fill(0, count($distancesFromPOI), 0));
                }
                $index = (int)$poi;
                $graph[$index] = $distancesFromPOI;
            }
        }
    }

    return dfs($graph, 0, [0 => true], true);
}

function bfsGetEdgeWeights($grid, $start) {
    $poiToDistance = [
        $grid[$start[0]][$start[1]] => 0,
    ];

    $queue = [
        ['row' => $start[0], 'col' => $start[1], 'distance' => 0],
    ];
    $visited = [];
    while (!empty($queue)) {
        $front = array_shift($queue);

        $key = $front['row'] . ',' . $front['col'];
        if (isset($visited[$key])) {
            continue;
        }
        $visited[$key] = true;

        if (preg_match('/[0-9]/', $grid[$front['row']][$front['col']])) {
            $poiToDistance[$grid[$front['row']][$front['col']]] = $front['distance'];
        }
        foreach ([[0, -1], [0, 1], [1, 0], [-1, 0]] as $d) {
            $nextRow = $front['row'] + $d[0];
            $nextCol = $front['col'] + $d[1];

            if ($grid[$nextRow][$nextCol] != '#') {
                $queue[] = ['row' => $nextRow, 'col' => $nextCol, 'distance' => $front['distance'] + 1];
            }
        }
    }

    $distances = [];
    foreach ($poiToDistance as $numStr => $dist) {
        $distances[(int)$numStr] = $dist;
    }
    return $distances;
}

function dfs($graph, $entryIndex, $visited, $returnToZero) {
    if (count($visited) == count($graph)) {
        if ($returnToZero) {
            return $graph[$entryIndex][0];
        }
        return 0;
    }

    $minDistance = PHP_INT_MAX;
    foreach ($graph[$entryIndex] as $i => $val) {
        if (!isset($visited[$i])) {
            $visited[$i] = true;

            $dist = $val + dfs($graph, $i, $visited, $returnToZero);
            $minDistance = min($minDistance, $dist);

            unset($visited[$i]);
        }
    }

    return $minDistance;
}

$input = file_get_contents('input.txt');
$result = cleaningRobot($input);
echo $result;