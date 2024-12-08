
<?php

$grid = [];
$file = fopen("input.txt", "r");
if ($file) {
    while (($line = fgets($file)) !== false) {
        $grid[] = str_split(trim($line));
    }
    fclose($file);
} else {
    die("Unable to open file!");
}

$h = count($grid);
$w = count($grid[0]);

$dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
$x = $y = $dirX = $dirY = $dirIdx = 0;
$found = false;

for ($i = 0; $i < $h && !$found; $i++) {
    for ($j = 0; $j < $w && !$found; $j++) {
        switch ($grid[$i][$j]) {
            case '^':
                $x = $j;
                $y = $i;
                $dirIdx = 0;
                break;
            case '>':
                $x = $j;
                $y = $i;
                $dirIdx = 1;
                break;
            case 'v':
                $x = $j;
                $y = $i;
                $dirIdx = 2;
                break;
            case '<':
                $x = $j;
                $y = $i;
                $dirIdx = 3;
                break;
        }
        if($x !== 0 || $y !== 0){
            $found = true;
            $dirX = $dirs[$dirIdx][0];
            $dirY = $dirs[$dirIdx][1];
        }
    }
}

$visited = [];
$visited[$y * $w + $x] = true;

while (true) {
    $nx = $x + $dirX;
    $ny = $y + $dirY;
    if ($nx < 0 || $nx >= $w || $ny < 0 || $ny >= $h) {
        break;
    }
    if ($grid[$ny][$nx] == '#') {
        $dirIdx = ($dirIdx + 1) % 4;
        $dirX = $dirs[$dirIdx][0];
        $dirY = $dirs[$dirIdx][1];
        continue;
    }
    $x = $nx;
    $y = $ny;
    $visited[$y * $w + $x] = true;
}

echo count($visited) . PHP_EOL;

?>
