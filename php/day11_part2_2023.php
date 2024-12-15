
<?php

function solve(string $filename, int $expansionFactor): int
{
    $lines = file($filename, FILE_IGNORE_NEW_LINES);
    $grid = array_map(fn($line) => str_split($line), $lines);
    $rows = count($grid);
    $cols = count($grid[0]);

    $emptyRows = [];
    for ($r = 0; $r < $rows; $r++) {
        if (strpos(implode('', $grid[$r]), '#') === false) {
            $emptyRows[] = $r;
        }
    }

    $emptyCols = [];
    for ($c = 0; $c < $cols; $c++) {
        $col = '';
        for ($r = 0; $r < $rows; $r++) {
            $col .= $grid[$r][$c];
        }
        if (strpos($col, '#') === false) {
            $emptyCols[] = $c;
        }
    }

    $galaxies = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === '#') {
                $galaxies[] = [$r, $c];
            }
        }
    }

    $totalDistance = 0;
    $galaxyCount = count($galaxies);
    for ($i = 0; $i < $galaxyCount; $i++) {
        for ($j = $i + 1; $j < $galaxyCount; $j++) {
            $r1 = $galaxies[$i][0];
            $c1 = $galaxies[$i][1];
            $r2 = $galaxies[$j][0];
            $c2 = $galaxies[$j][1];

            $rowExpansion = 0;
            foreach ($emptyRows as $row) {
                if (($row > min($r1, $r2)) && ($row < max($r1, $r2))) {
                    $rowExpansion += $expansionFactor - 1;
                }
            }

            $colExpansion = 0;
            foreach ($emptyCols as $col) {
                if (($col > min($c1, $c2)) && ($col < max($c1, $c2))) {
                    $colExpansion += $expansionFactor - 1;
                }
            }

            $distance = abs($r1 - $r2) + abs($c1 - $c2) + $rowExpansion + $colExpansion;
            $totalDistance += $distance;
        }
    }

    return $totalDistance;
}

$part1 = solve('input.txt', 2);
$part2 = solve('input.txt', 1000000);

echo "Part 1: " . $part1 . "\n";
echo "Part 2: " . $part2 . "\n";
?>
