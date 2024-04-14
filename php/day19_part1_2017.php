<?php

$grid = file("input.txt", FILE_IGNORE_NEW_LINES);

$x = strpos($grid[0], '|');
$y = 0;
$dx = 0;
$dy = 1;
$letters = '';

while (true) {
    if ($x < 0 || $x >= strlen($grid[0]) || $y < 0 || $y >= count($grid)) {
        break;
    }

    $cell = $grid[$y][$x] ?? ' ';

    if ($cell === ' ') {
        break;
    }

    if (ctype_upper($cell)) {
        $letters .= $cell;
    }

    if ($cell === '+') {
        if ($dx === 0) {
            if ($x > 0 && ($grid[$y][$x - 1] === '-' || ctype_upper($grid[$y][$x - 1]))) {
                $dx = -1;
                $dy = 0;
            } else {
                $dx = 1;
                $dy = 0;
            }
        } else {
            if ($y > 0 && ($grid[$y - 1][$x] === '|' || ctype_upper($grid[$y - 1][$x]))) {
                $dx = 0;
                $dy = -1;
            } else {
                $dx = 0;
                $dy = 1;
            }
        }
    }

    $x += $dx;
    $y += $dy;
}

echo $letters;