<?php
class Sensor {
    public $pos;
    public $beacon;
    public $dist;
}

function distress($sensors, $maxcoord) {
    for ($x = 0; $x <= $maxcoord; $x++) {
        for ($y = 0; $y <= $maxcoord; $y++) {
            $p = ['x' => $x, 'y' => $y];
            $detected = false;
            $skip = 0;
            foreach ($sensors as $s) {
                if (manhattan($s->pos, $p) <= $s->dist) {
                    $detected = true;
                    $dist = $s->dist - abs($s->pos['x'] - $x);
                    $skip = max($skip, $dist + $s->pos['y'] - $y);
                }
            }
            if (!$detected) {
                return $x * 4000000 + $y;
            }
            $y += $skip;
        }
    }
    return -1;
}

function readAll($path) {
    if (!file_exists($path)) {
        throw new Exception("File not found: $path");
    }
    return file_get_contents($path);
}

function manhattan($p, $q) {
    return abs($p['x'] - $q['x']) + abs($p['y'] - $q['y']);
}

$sensors = [];
$input = readAll('input.txt');
foreach (explode("\n", $input) as $line) {
    $s = new Sensor();
    sscanf($line, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d", $s->pos['x'], $s->pos['y'], $s->beacon['x'], $s->beacon['y']);
    $s->dist = manhattan($s->pos, $s->beacon);
    $sensors[] = $s;
}

echo distress($sensors, 4000000);