
<?php

$file = fopen("input.txt", "r");
$heightmap = [];
while (!feof($file)) {
    $line = trim(fgets($file));
    $row = [];
    for ($i = 0; $i < strlen($line); $i++) {
        $height = intval($line[$i]);
        $row[] = $height;
    }
    $heightmap[] = $row;
}
fclose($file);

$basinSizes = [];
$visited = [];

function isLowPoint($heightmap, $x, $y) {
    $height = $heightmap[$y][$x];
    if ($x > 0 && $heightmap[$y][$x-1] <= $height) {
        return false;
    }
    if ($x < count($heightmap[$y])-1 && $heightmap[$y][$x+1] <= $height) {
        return false;
    }
    if ($y > 0 && $heightmap[$y-1][$x] <= $height) {
        return false;
    }
    if ($y < count($heightmap)-1 && $heightmap[$y+1][$x] <= $height) {
        return false;
    }
    return true;
}

function exploreBasin($heightmap, $x, $y, &$visited) {
    global $basinSizes;
    if (isset($visited[$x][$y]) || $heightmap[$y][$x] == 9) {
        return 0;
    }
    $visited[$x][$y] = true;
    $size = 1;

    $directions = [[0, -1], [-1, 0], [0, 1], [1, 0]];
    foreach ($directions as $dir) {
        $newX = $x + $dir[0];
        $newY = $y + $dir[1];
        if ($newX >= 0 && $newX < count($heightmap[0]) && $newY >= 0 && $newY < count($heightmap)) {
            $size += exploreBasin($heightmap, $newX, $newY, $visited);
        }
    }
    return $size;
}

foreach ($heightmap as $y => $row) {
    foreach ($row as $x => $height) {
        if (isLowPoint($heightmap, $x, $y)) {
            $size = exploreBasin($heightmap, $x, $y, $visited);
            $basinSizes[] = $size;
        }
    }
}

rsort($basinSizes);
$result = $basinSizes[0] * $basinSizes[1] * $basinSizes[2];
echo $result . PHP_EOL;
?>
