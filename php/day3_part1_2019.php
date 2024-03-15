<?php

class Point {
    public $X, $Y;

    public function __construct($x, $y) {
        $this->X = $x;
        $this->Y = $y;
    }
}

function getPoints($path) {
    $points = [];
    $current = new Point(0, 0);
    $moves = explode(",", $path);
    foreach ($moves as $move) {
        $dir = $move[0];
        $steps = intval(substr($move, 1));
        for ($i = 0; $i < $steps; $i++) {
            switch ($dir) {
                case 'U':
                    $current->Y++;
                    break;
                case 'D':
                    $current->Y--;
                    break;
                case 'L':
                    $current->X--;
                    break;
                case 'R':
                    $current->X++;
                    break;
            }
            $points[$current->X . ',' . $current->Y] = true;
        }
    }
    return $points;
}

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$wire1 = getPoints($lines[0]);
$wire2 = getPoints($lines[1]);

$intersections = array_intersect_key($wire1, $wire2);

$minDistance = PHP_INT_MAX;
foreach ($intersections as $key => $value) {
    $coords = explode(',', $key);
    $distance = abs(intval($coords[0])) + abs(intval($coords[1]));
    if ($distance < $minDistance) {
        $minDistance = $distance;
    }
}

echo $minDistance . "\n";