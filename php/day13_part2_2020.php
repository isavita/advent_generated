
<?php

function readBusIDs($fileName) {
    $file = fopen($fileName, "r");
    fgets($file); // Skip the first line
    $busData = explode(",", trim(fgets($file)));

    $ids = [];
    $offsets = [];
    foreach ($busData as $i => $bus) {
        if ($bus !== "x") {
            $id = (int)$bus;
            $ids[] = $id;
            $offsets[] = $i;
        }
    }

    fclose($file);
    return [$ids, $offsets];
}

function extendedGCD($a, $b) {
    if ($a == 0) {
        return [0, 1];
    }
    list($x1, $y1) = extendedGCD($b % $a, $a);
    $x = $y1 - (int)($b / $a) * $x1;
    $y = $x1;
    return [$x, $y];
}

function findEarliestTimestamp($ids, $offsets) {
    $N = 1;
    foreach ($ids as $id) {
        $N *= $id;
    }

    $result = 0;
    foreach ($ids as $i => $id) {
        $ni = $N / $id;
        list($xi, $_) = extendedGCD($ni, $id);
        $result += (-$offsets[$i] + $id) % $id * $xi * $ni;
    }

    return $result % $N;
}

list($ids, $offsets) = readBusIDs("input.txt");
$timestamp = findEarliestTimestamp($ids, $offsets);
echo $timestamp . PHP_EOL;
?>
