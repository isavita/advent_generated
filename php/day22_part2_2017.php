<?php
class Position {
    public $x, $y;

    public function __construct($x, $y) {
        $this->x = $x;
        $this->y = $y;
    }

    public function getKey() {
        return "{$this->x},{$this->y}";
    }
}

define('CLEAN', 0);
define('WEAKENED', 1);
define('INFECTED', 2);
define('FLAGGED', 3);

$lines = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$grid = [];
$startX = $startY = 0;

foreach ($lines as $y => $line) {
    for ($x = 0; $x < strlen($line); $x++) {
        if ($line[$x] === '#') {
            $grid["{$x},{$y}"] = INFECTED;
        }
    }
    $startX = intval(strlen($line) / 2);
    $startY = intval(count($lines) / 2);
}

$dx = [0, 1, 0, -1];
$dy = [-1, 0, 1, 0];

$x = $startX;
$y = $startY;
$dir = 0;
$infectedCount = 0;

for ($i = 0; $i < 10000000; $i++) {
    $key = "{$x},{$y}";
    $current = $grid[$key] ?? CLEAN;
    switch ($current) {
        case CLEAN:
            $dir = ($dir - 1 + 4) % 4;
            $grid[$key] = WEAKENED;
            break;
        case WEAKENED:
            $grid[$key] = INFECTED;
            $infectedCount++;
            break;
        case INFECTED:
            $dir = ($dir + 1) % 4;
            $grid[$key] = FLAGGED;
            break;
        case FLAGGED:
            $dir = ($dir + 2) % 4;
            $grid[$key] = CLEAN;
            break;
    }
    $x += $dx[$dir];
    $y += $dy[$dir];
}

echo $infectedCount;
?>