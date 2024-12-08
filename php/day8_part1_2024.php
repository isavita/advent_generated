
<?php

$file = fopen("input.txt", "r");
$grid = [];
while (($line = fgets($file)) !== false) {
    $grid[] = trim($line);
}
fclose($file);

$h = count($grid);
$w = strlen($grid[0]);
$antennas = [];
foreach ($grid as $y => $row) {
    for ($x = 0; $x < $w; $x++) {
        $c = $row[$x];
        if ($c != '.') {
            $antennas[$c][] = [$y, $x];
        }
    }
}

$antinodes = [];
foreach ($antennas as $coords) {
    $n = count($coords);
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $A = $coords[$i];
            $B = $coords[$j];
            $P1 = [2 * $A[0] - $B[0], 2 * $A[1] - $B[1]];
            $P2 = [2 * $B[0] - $A[0], 2 * $B[1] - $A[1]];
            if ($P1[0] >= 0 && $P1[0] < $h && $P1[1] >= 0 && $P1[1] < $w) {
                $antinodes[serialize($P1)] = true;
            }
            if ($P2[0] >= 0 && $P2[0] < $h && $P2[1] >= 0 && $P2[1] < $w) {
                $antinodes[serialize($P2)] = true;
            }
        }
    }
}

echo count($antinodes) . PHP_EOL;

?>
