<?php

class Coord
{
    public $x, $y, $z;

    public function __construct($x, $y, $z)
    {
        $this->x = $x;
        $this->y = $y;
        $this->z = $z;
    }
}

class Point
{
    public $pos, $vel;

    public function __construct($pos, $vel)
    {
        $this->pos = $pos;
        $this->vel = $vel;
    }
}

function parseInput($input)
{
    $points = [];
    foreach ($input as $line) {
        $parts = explode('@', $line);
        $pos = explode(',', trim($parts[0]));
        $vel = explode(',', trim($parts[1]));
        $points[] = new Point(
            new Coord((float)$pos[0], (float)$pos[1], (float)$pos[2]),
            new Coord((float)$vel[0], (float)$vel[1], (float)$vel[2])
        );
    }
    return $points;
}

function isIntersecting2D($p1, $p2)
{
    $det = $p1->vel->x * $p2->vel->y - $p2->vel->x * $p1->vel->y;
    if ($det == 0) {
        return [false, new Coord(0, 0, 0), 0, 0];
    }
    $t1 = ($p2->vel->y * ($p2->pos->x - $p1->pos->x) - $p2->vel->x * ($p2->pos->y - $p1->pos->y)) / $det;
    $t2 = ($p1->vel->y * ($p2->pos->x - $p1->pos->x) - $p1->vel->x * ($p2->pos->y - $p1->pos->y)) / $det;
    $coord = new Coord(
        $p1->pos->x + $p1->vel->x * $t1,
        $p1->pos->y + $p1->vel->y * $t1,
        0
    );
    return [true, $coord, $t1, $t2];
}

function solve($input, $min, $max)
{
    $points = parseInput($input);
    $cnt = 0;
    for ($i = 0; $i < count($points); $i++) {
        for ($j = 0; $j < $i; $j++) {
            list($isIntersecting, $coord, $time1, $time2) = isIntersecting2D($points[$i], $points[$j]);
            $isInBound = $min <= $coord->x && $coord->x <= $max && $min <= $coord->y && $coord->y <= $max;
            if ($isIntersecting && $isInBound && $time1 >= 0 && $time2 >= 0) {
                $cnt++;
            }
        }
    }
    return $cnt;
}

function readInputFile($fileName)
{
    $file = file($fileName, FILE_IGNORE_NEW_LINES);
    return $file;
}

$input = readInputFile('input.txt');
echo solve($input, 200000000000000, 400000000000000) . "\n";