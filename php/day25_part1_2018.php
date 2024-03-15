<?php

class Point {
    public $x, $y, $z, $t;

    public function __construct($x, $y, $z, $t) {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
        $this->t = $t;
    }
}

function manhattanDistance($a, $b) {
    return abs($a->x - $b->x) + abs($a->y - $b->y) + abs($a->z - $b->z) + abs($a->t - $b->t);
}

class UnionFind {
    private $parent;

    public function __construct($size) {
        $this->parent = range(0, $size - 1);
    }

    public function find($x) {
        if ($this->parent[$x] != $x) {
            $this->parent[$x] = $this->find($this->parent[$x]);
        }
        return $this->parent[$x];
    }

    public function union($x, $y) {
        $rootX = $this->find($x);
        $rootY = $this->find($y);
        if ($rootX != $rootY) {
            $this->parent[$rootX] = $rootY;
        }
    }
}

$file = fopen("input.txt", "r");
$points = [];
while (($line = fgets($file)) !== false) {
    $coords = explode(",", trim($line));
    $points[] = new Point((int)$coords[0], (int)$coords[1], (int)$coords[2], (int)$coords[3]);
}
fclose($file);

$uf = new UnionFind(count($points));
for ($i = 0; $i < count($points); $i++) {
    for ($j = 0; $j < count($points); $j++) {
        if (manhattanDistance($points[$i], $points[$j]) <= 3) {
            $uf->union($i, $j);
        }
    }
}

$constellationCount = 0;
for ($i = 0; $i < count($points); $i++) {
    if ($uf->find($i) == $i) {
        $constellationCount++;
    }
}
echo $constellationCount;