
<?php
$grid = array_map('str_split', file('input.txt', FILE_IGNORE_NEW_LINES));

$x = array_search('|', $grid[0]);
$y = 0;
$dx = 0;
$dy = 1;
$steps = 0;

while (true) {
    if ($x < 0 || $x >= count($grid[0]) || $y < 0 || $y >= count($grid)) {
        break;
    }

    $cell = $grid[$y][$x];

    if ($cell === ' ') {
        break;
    }

    $steps++;

    if ($cell === '+') {
        if ($dx === 0) {
            $dx = ($x > 0 && ($grid[$y][$x - 1] === '-' || ctype_upper($grid[$y][$x - 1]))) ? -1 : 1;
            $dy = 0;
        } else {
            $dy = ($y > 0 && ($grid[$y - 1][$x] === '|' || ctype_upper($grid[$y - 1][$x]))) ? -1 : 1;
            $dx = 0;
        }
    }

    $x += $dx;
    $y += $dy;
}

echo $steps;
?>
