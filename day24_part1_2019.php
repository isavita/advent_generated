
<?php

const Side = 5;
const Square = Side * Side;

function parse()
{
    $res = array_fill(0, Square, false);

    $file = fopen("input.txt", "r");
    if ($file === false) {
        die("Error opening file");
    }

    $row = 0;
    while (($line = fgets($file)) !== false) {
        for ($col = 0; $col < Side; $col++) {
            if ($line[$col] === '#') {
                $res[$row * Side + $col] = true;
            }
        }
        $row++;
    }

    fclose($file);

    return $res;
}

function next1($grid)
{
    $newGrid = array_fill(0, Square, false);

    for ($i = 0; $i < Square; $i++) {
        $row = (int)($i / Side);
        $col = $i % Side;
        $neighbours = 0;

        if ($row > 0 && $grid[$i - Side]) {
            $neighbours++;
        }
        if ($row < Side - 1 && $grid[$i + Side]) {
            $neighbours++;
        }
        if ($col > 0 && $grid[$i - 1]) {
            $neighbours++;
        }
        if ($col < Side - 1 && $grid[$i + 1]) {
            $neighbours++;
        }

        if ($grid[$i] && $neighbours != 1) {
            $newGrid[$i] = false;
            continue;
        }

        if (!$grid[$i] && ($neighbours == 1 || $neighbours == 2)) {
            $newGrid[$i] = true;
            continue;
        }

        $newGrid[$i] = $grid[$i];
    }

    return $newGrid;
}

function biodiversity($grid)
{
    $bio = 0;
    for ($i = 0; $i < Square; $i++) {
        if ($grid[$i]) {
            $bio += 1 << $i;
        }
    }
    return $bio;
}

$appeared = [];
$grid = parse();
$appeared[serialize($grid)] = true;

while (true) {
    $grid = next1($grid);
    if (isset($appeared[serialize($grid)])) {
        echo biodiversity($grid) . PHP_EOL;
        break;
    }
    $appeared[serialize($grid)] = true;
}
