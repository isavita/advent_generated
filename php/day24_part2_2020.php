<?php

class Coordinate {
    public $q;
    public $r;

    public function __construct($q, $r) {
        $this->q = $q;
        $this->r = $r;
    }

    public function getKey() {
        return $this->q . "_" . $this->r;
    }
}

$directions = [
    "e"  => new Coordinate(1, 0),
    "se" => new Coordinate(0, 1),
    "sw" => new Coordinate(-1, 1),
    "w"  => new Coordinate(-1, 0),
    "nw" => new Coordinate(0, -1),
    "ne" => new Coordinate(1, -1)
];

function getNeighbors($tile) {
    global $directions;
    $neighbors = [];
    foreach ($directions as $dir) {
        $neighbors[] = new Coordinate($tile->q + $dir->q, $tile->r + $dir->r);
    }
    return $neighbors;
}

$blackTiles = [];
$lines = file("input.txt", FILE_IGNORE_NEW_LINES);

foreach ($lines as $line) {
    $coord = new Coordinate(0, 0);
    for ($i = 0; $i < strlen($line); $i++) {
        if ($line[$i] === 'e' || $line[$i] === 'w') {
            $dir = $line[$i];
        } else {
            $dir = $line[$i] . $line[$i + 1];
            $i++;
        }
        $move = $directions[$dir];
        $coord->q += $move->q;
        $coord->r += $move->r;
    }
    $key = $coord->getKey();
    $blackTiles[$key] = !isset($blackTiles[$key]) || !$blackTiles[$key];
}

for ($day = 0; $day < 100; $day++) {
    $tilesToCheck = [];
    foreach ($blackTiles as $key => $isBlack) {
        if ($isBlack) {
            $tile = explode("_", $key);
            $tileCoord = new Coordinate($tile[0], $tile[1]);
            $tilesToCheck[$key] = true;
            foreach (getNeighbors($tileCoord) as $neighbor) {
                $tilesToCheck[$neighbor->getKey()] = true;
            }
        }
    }

    $newBlackTiles = [];
    foreach ($tilesToCheck as $key => $_) {
        $tile = explode("_", $key);
        $tileCoord = new Coordinate($tile[0], $tile[1]);
        $blackNeighborCount = 0;
        foreach (getNeighbors($tileCoord) as $neighbor) {
            $neighborKey = $neighbor->getKey();
            if (isset($blackTiles[$neighborKey]) && $blackTiles[$neighborKey]) {
                $blackNeighborCount++;
            }
        }
        if ((isset($blackTiles[$key]) && $blackTiles[$key] && ($blackNeighborCount == 1 || $blackNeighborCount == 2)) ||
            (!isset($blackTiles[$key]) || !$blackTiles[$key]) && $blackNeighborCount == 2) {
            $newBlackTiles[$key] = true;
        }
    }

    $blackTiles = $newBlackTiles;
}

echo count($blackTiles) . "\n";