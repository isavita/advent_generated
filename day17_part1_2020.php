
<?php

$input = file_get_contents("input.txt");
$initialState = explode("\n", trim($input));
$activeCubes = [];

foreach ($initialState as $y => $line) {
    foreach (str_split($line) as $x => $char) {
        if ($char === '#') {
            $activeCubes["$x,$y,0"] = true;
        }
    }
}

for ($cycle = 0; $cycle < 6; $cycle++) {
    $activeCubes = simulateCycle($activeCubes);
}

echo count($activeCubes) . PHP_EOL;

function simulateCycle($activeCubes) {
    $newActiveCubes = [];
    $neighborCounts = [];

    foreach ($activeCubes as $coord => $_) {
        list($x, $y, $z) = explode(",", $coord);
        for ($dz = -1; $dz <= 1; $dz++) {
            for ($dy = -1; $dy <= 1; $dy++) {
                for ($dx = -1; $dx <= 1; $dx++) {
                    if ($dz == 0 && $dy == 0 && $dx == 0) {
                        continue;
                    }
                    $neighbor = ($x + $dx) . "," . ($y + $dy) . "," . ($z + $dz);
                    if (!isset($neighborCounts[$neighbor])) {
                        $neighborCounts[$neighbor] = 0;
                    }
                    $neighborCounts[$neighbor]++;
                }
            }
        }
    }

    foreach ($neighborCounts as $coord => $count) {
        list($x, $y, $z) = explode(",", $coord);
        if ($count == 3 || ($count == 2 && isset($activeCubes["$x,$y,$z"]))) {
            $newActiveCubes["$x,$y,$z"] = true;
        }
    }

    return $newActiveCubes;
}
?>
