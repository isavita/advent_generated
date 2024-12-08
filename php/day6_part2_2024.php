
<?php

$grid = [];
$file = fopen("input.txt", "r");
while (($line = fgets($file)) !== false) {
    $grid[] = str_split(trim($line));
}
fclose($file);

$h = count($grid);
$w = count($grid[0]);

$startX = $startY = $startDir = -1;
foreach ($grid as $y => $row) {
    foreach ($row as $x => $cell) {
        switch ($cell) {
            case '^': $startX = $x; $startY = $y; $startDir = 0; break;
            case '>': $startX = $x; $startY = $y; $startDir = 1; break;
            case 'v': $startX = $x; $startY = $y; $startDir = 2; break;
            case '<': $startX = $x; $startY = $y; $startDir = 3; break;
        }
    }
}
$grid[$startY][$startX] = '.';

$canLoop = 0;
for ($y = 0; $y < $h; $y++) {
    for ($x = 0; $x < $w; $x++) {
        if ($x == $startX && $y == $startY) continue;
        if ($grid[$y][$x] != '.') continue;
        $grid[$y][$x] = '#';
        if (loops($grid, $startX, $startY, $startDir)) $canLoop++;
        $grid[$y][$x] = '.';
    }
}

echo $canLoop . PHP_EOL;

function loops($grid, $sx, $sy, $sdir) {
    $h = count($grid);
    $w = count($grid[0]);
    $dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    $x = $sx;
    $y = $sy;
    $dir = $sdir;
    $seen = [];
    for ($step = 0; $step < 2000000; $step++) {
        $st = serialize([$x, $y, $dir]);
        if (isset($seen[$st])) return true;
        $seen[$st] = true;
        $dx = $dirs[$dir][0];
        $dy = $dirs[$dir][1];
        $nx = $x + $dx;
        $ny = $y + $dy;
        if ($nx < 0 || $nx >= $w || $ny < 0 || $ny >= $h) return false;
        if ($grid[$ny][$nx] == '#') {
            $dir = ($dir + 1) % 4;
            continue;
        }
        $x = $nx;
        $y = $ny;
    }
    return false;
}

?>
