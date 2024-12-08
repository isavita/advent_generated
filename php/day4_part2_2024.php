
<?php

function checkMAS(array $grid, int $x, int $y, int $dx, int $dy): bool
{
    $word = "MAS";
    $len = strlen($word);
    $forward = true;
    $backward = true;

    for ($i = 0; $i < $len; $i++) {
        $newX = $x + ($dx * $i);
        $newY = $y + ($dy * $i);
        if ($newX < 0 || $newY < 0 || $newX >= count($grid) || $newY >= strlen($grid[$newX])) {
            $forward = false;
            break;
        }
        if ($grid[$newX][$newY] !== $word[$i]) {
            $forward = false;
        }
    }

    for ($i = 0; $i < $len; $i++) {
        $newX = $x + ($dx * $i);
        $newY = $y + ($dy * $i);
        if ($newX < 0 || $newY < 0 || $newX >= count($grid) || $newY >= strlen($grid[$newX])) {
            $backward = false;
            break;
        }
        if ($grid[$newX][$newY] !== $word[$len - 1 - $i]) {
            $backward = false;
        }
    }

    return $forward || $backward;
}

function checkXMAS(array $grid, int $x, int $y): bool
{
    return (checkMAS($grid, $x - 1, $y - 1, 1, 1) && checkMAS($grid, $x - 1, $y + 1, 1, -1)) ||
           (checkMAS($grid, $x + 1, $y - 1, -1, 1) && checkMAS($grid, $x + 1, $y + 1, -1, -1));
}

function countXMASPatterns(array $grid): int
{
    $count = 0;
    $rows = count($grid);
    $cols = strlen($grid[0]);

    if ($rows < 3 || $cols < 3) {
        return 0;
    }

    for ($i = 1; $i < $rows - 1; $i++) {
        for ($j = 1; $j < $cols -1; $j++) {
            if ($grid[$i][$j] === 'A' && checkXMAS($grid, $i, $j)) {
                $count++;
            }
        }
    }

    return $count;
}


$filename = "input.txt";
$grid = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

if ($grid === false) {
    die("Error opening file: $filename");
}

echo "X-MAS patterns appear " . countXMASPatterns($grid) . " times in the word search\n";

?>
