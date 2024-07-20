
<?php

function readAsteroidMap($filename) {
    return array_map('str_split', file($filename, FILE_IGNORE_NEW_LINES));
}

function getVisibleAsteroids($map, $x, $y) {
    $asteroids = [];
    $width = count($map[0]);
    $height = count($map);

    for ($i = 0; $i < $height; $i++) {
        for ($j = 0; $j < $width; $j++) {
            if ($map[$i][$j] === '#' && !($i === $y && $j === $x)) {
                $dx = $j - $x;
                $dy = $i - $y;
                $gcd = gcd(abs($dx), abs($dy));
                $dx /= $gcd;
                $dy /= $gcd;
                $asteroids["$dx,$dy"][] = [$j, $i];
            }
        }
    }

    return count($asteroids);
}

function gcd($a, $b) {
    while ($b) {
        $temp = $b;
        $b = $a % $b;
        $a = $temp;
    }
    return $a;
}

function findBestLocation($map) {
    $bestCount = 0;
    $bestLocation = null;

    foreach ($map as $y => $row) {
        foreach ($row as $x => $cell) {
            if ($cell === '#') {
                $visibleCount = getVisibleAsteroids($map, $x, $y);
                if ($visibleCount > $bestCount) {
                    $bestCount = $visibleCount;
                    $bestLocation = [$x, $y];
                }
            }
        }
    }

    return [$bestLocation, $bestCount];
}

function vaporizeAsteroids($map, $station) {
    $xStation = $station[0];
    $yStation = $station[1];
    $asteroids = [];
    $width = count($map[0]);
    $height = count($map);

    for ($i = 0; $i < $height; $i++) {
        for ($j = 0; $j < $width; $j++) {
            if ($map[$i][$j] === '#' && !($i === $yStation && $j === $xStation)) {
                $dx = $j - $xStation;
                $dy = $i - $yStation;
                $angle = atan2($dy, $dx);
                $distance = sqrt($dx * $dx + $dy * $dy);
                $asteroids[$angle][] = [$distance, $j, $i];
            }
        }
    }

    foreach ($asteroids as &$group) {
        usort($group, function($a, $b) {
            return $a[0] <=> $b[0]; // Sort by distance
        });
    }

    ksort($asteroids); // Sort by angle

    $vaporized = [];
    while (count($vaporized) < 200) {
        foreach ($asteroids as $angle => &$group) {
            if (!empty($group)) {
                $vaporized[] = array_shift($group);
            }
        }
    }

    return $vaporized[199]; // Return the 200th vaporized asteroid
}

$map = readAsteroidMap('input.txt');
list($bestLocation, $bestCount) = findBestLocation($map);
echo "Best location: (" . implode(',', $bestLocation) . ") with $bestCount visible asteroids.\n";

$vaporizedAsteroid = vaporizeAsteroids($map, $bestLocation);
$result = $vaporizedAsteroid[1] * 100 + $vaporizedAsteroid[2];
echo "The 200th asteroid to be vaporized is at (" . implode(',', [$vaporizedAsteroid[1], $vaporizedAsteroid[2]]) . ") with result: $result.\n";
?>
