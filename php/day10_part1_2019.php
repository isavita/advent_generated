
<?php

function readAsteroidMap($filename) {
    $lines = file($filename, FILE_IGNORE_NEW_LINES);
    return array_map('str_split', $lines);
}

function gcd($a, $b) {
    while ($b) {
        $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

function countVisibleAsteroids($map, $x, $y) {
    $asteroids = [];
    $rows = count($map);
    $cols = count($map[0]);

    // Collect all asteroid positions
    for ($i = 0; $i < $rows; $i++) {
        for ($j = 0; $j < $cols; $j++) {
            if ($map[$i][$j] === '#') {
                $asteroids[] = [$j, $i]; // Store as (x, y)
            }
        }
    }

    $visible = [];

    foreach ($asteroids as [$ax, $ay]) {
        if ($ax === $x && $ay === $y) {
            continue; // Skip the current asteroid
        }

        // Calculate the differences
        $dx = $ax - $x;
        $dy = $ay - $y;

        // Reduce dx and dy to their simplest form
        $g = gcd(abs($dx), abs($dy));
        $dx /= $g;
        $dy /= $g;

        // Check if there's an asteroid blocking the line of sight
        $blocked = false;
        $tx = $x + $dx;
        $ty = $y + $dy;

        while ($tx !== $ax || $ty !== $ay) {
            if ($tx >= 0 && $tx < $cols && $ty >= 0 && $ty < $rows) {
                if ($map[$ty][$tx] === '#') {
                    $blocked = true;
                    break;
                }
            }
            $tx += $dx;
            $ty += $dy;
        }

        if (!$blocked) {
            $visible["$dx,$dy"] = true; // Store direction as unique key
        }
    }

    return count($visible);
}

function findBestLocation($map) {
    $bestCount = 0;
    $bestLocation = null;

    for ($y = 0; $y < count($map); $y++) {
        for ($x = 0; $x < count($map[0]); $x++) {
            if ($map[$y][$x] === '#') {
                $visibleCount = countVisibleAsteroids($map, $x, $y);
                if ($visibleCount > $bestCount) {
                    $bestCount = $visibleCount;
                    $bestLocation = [$x, $y];
                }
            }
        }
    }

    return [$bestLocation, $bestCount];
}

// Main execution
$map = readAsteroidMap('input.txt');
list($bestLocation, $maxVisible) = findBestLocation($map);

echo "Best location: (" . implode(',', $bestLocation) . ") with " . $maxVisible . " other asteroids visible.\n";
