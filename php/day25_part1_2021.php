
<?php

$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$grid = [];

foreach ($lines as $line) {
    $grid[] = str_split($line);
}

echo findSafeStep($grid) . PHP_EOL;

function findSafeStep($grid) {
    $step = 0;
    while (true) {
        $eastMoved = moveEast($grid);
        $southMoved = moveSouth($grid);
        $step++;

        if (!$eastMoved && !$southMoved) {
            break;
        }
    }
    return $step;
}

function moveEast(&$grid) {
    $moved = false;
    $height = count($grid);
    $width = count($grid[0]);

    $oldPositions = array_fill(0, $height, array_fill(0, $width, ''));
    for ($y = 0; $y < $height; $y++) {
        for ($x = 0; $x < $width; $x++) {
            if ($grid[$y][$x] == '>') {
                $nextX = ($x + 1) % $width;
                if ($grid[$y][$nextX] == '.') {
                    $oldPositions[$y][$x] = '.';
                    $grid[$y][$nextX] = '>';
                    $x++;
                    $moved = true;
                }
            }
        }
    }
    freeEmptyPositions($grid, $oldPositions);

    return $moved;
}

function moveSouth(&$grid) {
    $moved = false;
    $height = count($grid);
    $width = count($grid[0]);

    $oldPositions = array_fill(0, $height, array_fill(0, $width, ''));
    for ($x = 0; $x < $width; $x++) {
        for ($y = 0; $y < $height; $y++) {
            if ($grid[$y][$x] == 'v') {
                $nextY = ($y + 1) % $height;
                if ($grid[$nextY][$x] == '.') {
                    $oldPositions[$y][$x] = '.';
                    $grid[$nextY][$x] = 'v';
                    $y++;
                    $moved = true;
                }
            }
        }
    }
    freeEmptyPositions($grid, $oldPositions);

    return $moved;
}

function freeEmptyPositions(&$grid, $oldPostion) {
    $height = count($grid);
    $width = count($grid[0]);
    for ($y = 0; $y < $height; $y++) {
        for ($x = 0; $x < $width; $x++) {
            if ($oldPostion[$y][$x] == '.') {
                $grid[$y][$x] = '.';
            }
        }
    }
}
?>
