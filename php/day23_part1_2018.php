<?php

class Nanobot {
    public $X, $Y, $Z, $Radius;

    public function __construct($x, $y, $z, $radius) {
        $this->X = $x;
        $this->Y = $y;
        $this->Z = $z;
        $this->Radius = $radius;
    }
}

function parseNanobots($filename) {
    $nanobots = [];
    $file = fopen($filename, 'r');
    if ($file) {
        while (($line = fgets($file)) !== false) {
            preg_match('/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/', $line, $matches);
            $nanobots[] = new Nanobot(intval($matches[1]), intval($matches[2]), intval($matches[3]), intval($matches[4]));
        }
        fclose($file);
    } else {
        die("Unable to open file: $filename");
    }
    return $nanobots;
}

function findStrongestNanobot($nanobots) {
    $strongest = $nanobots[0];
    foreach ($nanobots as $nanobot) {
        if ($nanobot->Radius > $strongest->Radius) {
            $strongest = $nanobot;
        }
    }
    return $strongest;
}

function countNanobotsInRange($nanobots, $strongest) {
    $count = 0;
    foreach ($nanobots as $nanobot) {
        if (manhattanDistance($nanobot, $strongest) <= $strongest->Radius) {
            $count++;
        }
    }
    return $count;
}

function manhattanDistance($a, $b) {
    return abs($a->X - $b->X) + abs($a->Y - $b->Y) + abs($a->Z - $b->Z);
}

$nanobots = parseNanobots('input.txt');
$strongest = findStrongestNanobot($nanobots);
$inRangeCount = countNanobotsInRange($nanobots, $strongest);
echo $inRangeCount . "\n";