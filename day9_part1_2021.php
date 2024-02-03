
<?php

$file = fopen("input.txt", "r");
$heightmap = [];
while ($line = fgets($file)) {
    $row = [];
    $chars = str_split(trim($line));
    foreach ($chars as $char) {
        $height = intval($char);
        $row[] = $height;
    }
    $heightmap[] = $row;
}
fclose($file);

$totalRiskLevel = 0;
foreach ($heightmap as $y => $row) {
    foreach ($row as $x => $height) {
        if (isLowPoint($heightmap, $x, $y)) {
            $totalRiskLevel += 1 + $height;
        }
    }
}

echo $totalRiskLevel . PHP_EOL;

function isLowPoint($heightmap, $x, $y) {
    $height = $heightmap[$y][$x];
    if ($x > 0 && $heightmap[$y][$x - 1] <= $height) {
        return false;
    }
    if ($x < count($heightmap[$y]) - 1 && $heightmap[$y][$x + 1] <= $height) {
        return false;
    }
    if ($y > 0 && $heightmap[$y - 1][$x] <= $height) {
        return false;
    }
    if ($y < count($heightmap) - 1 && $heightmap[$y + 1][$x] <= $height) {
        return false;
    }
    return true;
}
?>
