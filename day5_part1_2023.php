
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$seeds = [];
$currentRanges = [];
$maps = [];

foreach ($lines as $line) {
    if (strpos($line, "map:") !== false) {
        if (count($currentRanges) > 0) {
            $maps[] = $currentRanges;
            $currentRanges = [];
        }
    } elseif (strpos($line, "seeds:") === 0) {
        $seedStrs = explode(" ", substr($line, 7));
        foreach ($seedStrs as $s) {
            $seeds[] = intval($s);
        }
    } else {
        $numbers = explode(" ", $line);
        if (count($numbers) == 3) {
            $srcStart = intval($numbers[1]);
            $destStart = intval($numbers[0]);
            $length = intval($numbers[2]);

            $currentRanges[] = ["srcStart" => $srcStart, "destStart" => $destStart, "length" => $length];
        }
    }
}
$maps[] = $currentRanges;

$minLocation = -1;
foreach ($seeds as $seed) {
    $location = $seed;
    foreach ($maps as $m) {
        $location = convertNumber($location, $m);
    }

    if ($minLocation == -1 || $location < $minLocation) {
        $minLocation = $location;
    }
}

echo $minLocation . "\n";

function convertNumber($number, $ranges) {
    foreach ($ranges as $r) {
        if ($number >= $r["srcStart"] && $number < $r["srcStart"] + $r["length"]) {
            return $r["destStart"] + ($number - $r["srcStart"]);
        }
    }
    return $number;
}
?>
