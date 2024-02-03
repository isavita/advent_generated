
<?php

$data = file_get_contents("input.txt");
list($depth, $target) = parseInput($data);

$cave = makeCaveSystem($depth, $target);
$riskLevel = calculateRiskLevel($cave, $target);
echo "Total Risk Level: $riskLevel\n";

function parseInput($data) {
    $lines = explode("\n", $data);
    $depth = explode(" ", $lines[0])[1];
    $coords = explode(" ", $lines[1])[1];
    list($x, $y) = explode(",", $coords);
    return array($depth, array($x, $y));
}

function makeCaveSystem($depth, $target) {
    $cave = [];
    for ($y = 0; $y <= $target[1]; $y++) {
        $cave[$y] = [];
        for ($x = 0; $x <= $target[0]; $x++) {
            if (($x == 0 && $y == 0) || ($x == $target[0] && $y == $target[1])) {
                $geologicIndex = 0;
            } elseif ($y == 0) {
                $geologicIndex = $x * 16807;
            } elseif ($x == 0) {
                $geologicIndex = $y * 48271;
            } else {
                $geologicIndex = $cave[$y][$x-1] * $cave[$y-1][$x];
            }
            $cave[$y][$x] = ($geologicIndex + $depth) % 20183;
        }
    }
    return $cave;
}

function calculateRiskLevel($cave, $target) {
    $riskLevel = 0;
    for ($y = 0; $y <= $target[1]; $y++) {
        for ($x = 0; $x <= $target[0]; $x++) {
            $riskLevel += $cave[$y][$x] % 3;
        }
    }
    return $riskLevel;
}
?>
