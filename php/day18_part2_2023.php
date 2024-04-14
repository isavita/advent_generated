<?php

class Coord {
    public $x;
    public $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function add($coord) {
        return new Coord($this->x + $coord->x, $this->y + $coord->y);
    }

    public function multiplyByScalar($scalar) {
        return new Coord($this->x * $scalar, $this->y * $scalar);
    }
}

$north = new Coord(0, -1);
$west = new Coord(-1, 0);
$south = new Coord(0, 1);
$east = new Coord(1, 0);

function absInt($x) {
    return $x < 0 ? -$x : $x;
}

function parseInput($input) {
    global $north, $west, $south, $east;
    $current = new Coord(0, 0);
    $vertices = [$current];

    foreach ($input as $line) {
        $parts = explode(" ", $line);
        $color = $parts[2];
        $dirInput = $color[7];
        $lengthStr = substr($color, 2, 5);
        $length = hexdec($lengthStr);

        switch ($dirInput) {
            case '3':
                $dir = $north;
                break;
            case '2':
                $dir = $west;
                break;
            case '1':
                $dir = $south;
                break;
            case '0':
                $dir = $east;
                break;
        }

        $current = $current->add($dir->multiplyByScalar($length));
        $vertices[] = $current;
    }

    return $vertices;
}

function shoelace($vertices) {
    $n = count($vertices);
    $area = 0;

    for ($i = 0; $i < $n; $i++) {
        $next = ($i + 1) % $n;
        $area += $vertices[$i]->x * $vertices[$next]->y;
        $area -= $vertices[$i]->y * $vertices[$next]->x;
    }

    return absInt($area) / 2;
}

function perimeter($vertices) {
    $n = count($vertices);
    $perim = 0;

    for ($i = 0; $i < $n; $i++) {
        $next = ($i + 1) % $n;
        $perim += absInt($vertices[$i]->x - $vertices[$next]->x) + absInt($vertices[$i]->y - $vertices[$next]->y);
    }

    return $perim;
}

function calculatePolygonArea($vertices) {
    return shoelace($vertices) + perimeter($vertices) / 2 + 1;
}

function solve($input) {
    $vertices = parseInput($input);
    return calculatePolygonArea($vertices);
}

function readInputFile($fileName) {
    $fileContent = file_get_contents($fileName);
    return explode("\n", trim($fileContent));
}

$input = readInputFile("input.txt");
echo solve($input);
?>