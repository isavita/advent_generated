
<?php

$rules = [];
$grid = [
    ".#.",
    "..#",
    "###",
];

$file = fopen("input.txt", "r");
while (!feof($file)) {
    $parts = explode(" => ", trim(fgets($file)));
    $rules[$parts[0]] = $parts[1];
}
fclose($file);

$newSize = 0;
$subSize = 0;

for ($i = 0; $i < 18; $i++) {
    if (count($grid) % 2 == 0) {
        $subSize = 2;
        $newSize = count($grid) / 2 * 3;
    } else {
        $subSize = 3;
        $newSize = count($grid) / 3 * 4;
    }

    $newGrid = array_fill(0, $newSize, "");

    for ($x = 0; $x < $newSize; $x++) {
        $newGrid[$x] = "";
    }

    for ($y = 0; $y < count($grid); $y += $subSize) {
        for ($x = 0; $x < count($grid); $x += $subSize) {
            $square = array();
            for ($dy = 0; $dy < $subSize; $dy++) {
                $square[] = substr($grid[$y + $dy], $x, $subSize);
            }
            $newSquare = enhance(implode("/", $square), $rules);
            $newSquareRows = explode("/", $newSquare);
            foreach ($newSquareRows as $dy => $row) {
                $newGrid[$y / $subSize * ($subSize + 1) + $dy] .= $row;
            }
        }
    }
    $grid = $newGrid;
}

$count = 0;
foreach ($grid as $row) {
    foreach (str_split($row) as $pixel) {
        if ($pixel == '#') {
            $count++;
        }
    }
}
echo $count . PHP_EOL;

function enhance($input, $rules) {
    static $memo = [];

    if (array_key_exists($input, $memo)) {
        return $memo[$input];
    }

    $original = $input;
    for ($i = 0; $i < 4; $i++) {
        if (array_key_exists($input, $rules)) {
            $memo[$original] = $rules[$input];
            return $rules[$input];
        }
        $input = rotate($input);
    }

    $input = flip($original);
    for ($i = 0; $i < 4; $i++) {
        if (array_key_exists($input, $rules)) {
            $memo[$original] = $rules[$input];
            return $rules[$input];
        }
        $input = rotate($input);
    }

    return "";
}

function rotate($input) {
    $parts = explode("/", $input);
    $size = count($parts);
    $newParts = array();

    for ($x = 0; $x < $size; $x++) {
        $newRow = "";
        for ($y = $size - 1; $y >= 0; $y--) {
            $newRow .= $parts[$y][$x];
        }
        $newParts[] = $newRow;
    }

    return implode("/", $newParts);
}

function flip($input) {
    $parts = explode("/", $input);
    foreach ($parts as $key => $part) {
        $parts[$key] = strrev($part);
    }
    return implode("/", $parts);
}
?>
