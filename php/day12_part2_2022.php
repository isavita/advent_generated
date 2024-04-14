<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$grid = [];
$start = $end = null;
$as = [];

foreach ($lines as $y => $line) {
    for ($x = 0; $x < strlen($line); $x++) {
        $point = ["x" => $x, "y" => $y];
        $char = $line[$x];
        $grid[$y][$x] = $char;
        if ($char == 'S') {
            $start = $point;
        } elseif ($char == 'E') {
            $end = $point;
        } elseif ($char == 'a') {
            $as[] = $point;
        }
    }
}

$grid[$start['y']][$start['x']] = 'a';
$grid[$end['y']][$end['x']] = 'z';

function djikstra($grid, $end) {
    $pq = new SplPriorityQueue();
    $dist = [];
    $dist[$end['y']][$end['x']] = 0;
    $pq->insert($end, 0);
    $neighbors4 = [[0, 1], [0, -1], [1, 0], [-1, 0]];

    while (!$pq->isEmpty()) {
        $current = $pq->extract();
        foreach ($neighbors4 as $n) {
            $next = ['x' => $current['x'] + $n[0], 'y' => $current['y'] + $n[1]];
            if (!isset($grid[$next['y']][$next['x']])) {
                continue;
            }
            if (ord($grid[$current['y']][$current['x']]) - ord($grid[$next['y']][$next['x']]) > 1) {
                continue;
            }
            $nextDist = $dist[$current['y']][$current['x']] + 1;
            if (!isset($dist[$next['y']][$next['x']]) || $nextDist < $dist[$next['y']][$next['x']]) {
                $dist[$next['y']][$next['x']] = $nextDist;
                $pq->insert($next, -$nextDist); // Negative because PHP's priority queue is max-heap by default
            }
        }
    }
    return $dist;
}

$dists = djikstra($grid, $end);
$l = $dists[$start['y']][$start['x']];

foreach ($as as $a) {
    if (isset($dists[$a['y']][$a['x']])) {
        $l = min($l, $dists[$a['y']][$a['x']]);
    }
}

echo $l;

?>