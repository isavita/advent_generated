
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", trim($input));

$orbitMap = [];
foreach ($lines as $line) {
    $parts = explode(")", $line);
    $center = $parts[0];
    $orbiter = $parts[1];
    $orbitMap[$center][] = $orbiter;
}

function countOrbits($orbitMap, $start, $depth) {
    if (!isset($orbitMap[$start])) {
        return $depth;
    }
    $orbits = $orbitMap[$start];
    $count = $depth;
    foreach ($orbits as $orbit) {
        $count += countOrbits($orbitMap, $orbit, $depth + 1);
    }
    return $count;
}

$totalOrbits = countOrbits($orbitMap, "COM", 0);
echo $totalOrbits . "\n";
?>
