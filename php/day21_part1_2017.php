
<?php

$rules = [];

$file = fopen("input.txt", "r");
while (!feof($file)) {
    $parts = explode(" => ", trim(fgets($file)));
    $rules[$parts[0]] = $parts[1];
}
fclose($file);

$grid = [
    ".#.",
    "..#",
    "###",
];

for ($i = 0; $i < 5; $i++) {
    $newSize = 0;
    $subSize = 0;

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
            $square = [];
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
    $count += substr_count($row, "#");
}
echo $count . PHP_EOL;

function enhance($input, $rules) {
    for ($i = 0; $i < 4; $i++) {
        if (array_key_exists($input, $rules)) {
            return $rules[$input];
        }
        $input = rotate($input);
    }
    $input = flip($input);
    for ($i = 0; $i < 4; $i++) {
        if (array_key_exists($input, $rules)) {
            return $rules[$input];
        }
        $input = rotate($input);
    }
    return "";
}

function rotate($input) {
    $parts = explode("/", $input);
    $size = count($parts);
    $newParts = [];
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
    foreach ($parts as $i => $part) {
        $parts[$i] = strrev($part);
    }
    return implode("/", $parts);
}
?>
