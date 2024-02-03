
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$polymer = $lines[0];
$rules = [];

for ($i = 1; $i < count($lines); $i++) {
    $line = $lines[$i];
    if ($line == "") {
        continue;
    }
    $parts = explode(" -> ", $line);
    $rules[$parts[0]] = $parts[1];
}

for ($step = 0; $step < 10; $step++) {
    $polymer = applyInsertion($polymer, $rules);
}

$counts = countElements($polymer);
list($min, $max) = minMax($counts);

echo $max - $min . "\n";

function applyInsertion($polymer, $rules) {
    $newPolymer = "";
    for ($i = 0; $i < strlen($polymer) - 1; $i++) {
        $newPolymer .= $polymer[$i];
        $pair = substr($polymer, $i, 2);
        if (array_key_exists($pair, $rules)) {
            $newPolymer .= $rules[$pair];
        }
    }
    $newPolymer .= $polymer[strlen($polymer) - 1];
    return $newPolymer;
}

function countElements($polymer) {
    $counts = [];
    for ($i = 0; $i < strlen($polymer); $i++) {
        $c = $polymer[$i];
        if (!array_key_exists($c, $counts)) {
            $counts[$c] = 1;
        } else {
            $counts[$c]++;
        }
    }
    return $counts;
}

function minMax($counts) {
    $min = PHP_INT_MAX;
    $max = PHP_INT_MIN;
    foreach ($counts as $count) {
        if ($count < $min) {
            $min = $count;
        }
        if ($count > $max) {
            $max = $count;
        }
    }
    return [$min, $max];
}
?>
