
<?php

function gcd($a, $b) {
    return $b ? gcd($b, $a % $b) : abs($a);
}

$grid = file('input.txt', FILE_IGNORE_NEW_LINES);
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

$linesPerFreq = [];
foreach ($antennas as $f => $coords) {
    $linesPerFreq[$f] = [];
    $n = count($coords);
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $A = $coords[$i];
            $B = $coords[$j];
            $dy = $B[0] - $A[0];
            $dx = $B[1] - $A[1];
            $g = gcd($dy, $dx);
            $sy = $dy / $g;
            $sx = $dx / $g;
            if ($sx < 0 || ($sx == 0 && $sy < 0)) {
                $sx = -$sx;
                $sy = -$sy;
            }
            $c = $sy * $A[1] - $sx * $A[0];
            $key = "$sx,$sy,$c";
            $linesPerFreq[$f][$key] = true;
        }
    }
}

$antinodes = [];
foreach ($linesPerFreq as $lines) {
    foreach ($lines as $key => $val) {
        sscanf($key, "%d,%d,%d", $sx, $sy, $c);
        if ($sx == 0 && $sy == 0) continue;
        if ($sy == 0) {
            if ($c % $sx == 0) {
                $y = -$c / $sx;
                if ($y >= 0 && $y < $h) {
                    for ($x = 0; $x < $w; $x++) {
                        $antinodes[$y . ',' . $x] = true;
                    }
                }
            }
        } elseif ($sx == 0) {
            if ($c % $sy == 0) {
                $x = $c / $sy;
                if ($x >= 0 && $x < $w) {
                    for ($y = 0; $y < $h; $y++) {
                        $antinodes[$y . ',' . $x] = true;
                    }
                }
            }
        } else {
            for ($y = 0; $y < $h; $y++) {
                $val = $c + $sx * $y;
                if ($val % $sy == 0) {
                    $x = $val / $sy;
                    if ($x >= 0 && $x < $w) {
                        $antinodes[$y . ',' . $x] = true;
                    }
                }
            }
        }
    }
}

echo count($antinodes) . PHP_EOL;

?>
