<?php

function readHappinessValues($filename) {
    $file = fopen($filename, "r");
    if (!$file) {
        return [null, "Error opening file"];
    }

    $happinessMap = [];
    while (($line = fgets($file)) !== false) {
        $parts = preg_split('/\s+/', trim($line));
        if (count($parts) < 11) {
            continue; // Skip invalid lines
        }

        $from = $parts[0];
        $to = rtrim($parts[10], '.');
        $change = intval($parts[3]);
        if ($parts[2] === "lose") {
            $change = -$change;
        }

        if (!isset($happinessMap[$from])) {
            $happinessMap[$from] = [];
        }
        $happinessMap[$from][$to] = $change;
    }
    fclose($file);

    return [$happinessMap, null];
}

function addYourself(&$happinessMap) {
    $happinessMap["You"] = [];
    foreach (array_keys($happinessMap) as $guest) {
        $happinessMap[$guest]["You"] = 0;
        $happinessMap["You"][$guest] = 0;
    }
}

function getGuestList($happinessMap) {
    return array_keys($happinessMap);
}

function calculateOptimalArrangement($guests, $happinessMap) {
    $maxHappiness = 0;
    permute($guests, 0, $maxHappiness, $happinessMap);
    return $maxHappiness;
}

function permute(&$arr, $i, &$maxHappiness, $happinessMap) {
    if ($i === count($arr)) {
        $happiness = calculateHappiness($arr, $happinessMap);
        if ($happiness > $maxHappiness) {
            $maxHappiness = $happiness;
        }
        return;
    }

    for ($j = $i; $j < count($arr); $j++) {
        [$arr[$i], $arr[$j]] = [$arr[$j], $arr[$i]];
        permute($arr, $i + 1, $maxHappiness, $happinessMap);
        [$arr[$i], $arr[$j]] = [$arr[$j], $arr[$i]];
    }
}

function calculateHappiness($arrangement, $happinessMap) {
    $happiness = 0;
    $n = count($arrangement);
    for ($i = 0; $i < $n; $i++) {
        $left = ($i + $n - 1) % $n;
        $right = ($i + 1) % $n;
        $happiness += $happinessMap[$arrangement[$i]][$arrangement[$left]];
        $happiness += $happinessMap[$arrangement[$i]][$arrangement[$right]];
    }
    return $happiness;
}

list($happinessMap, $error) = readHappinessValues("input.txt");
if ($error) {
    echo "Error reading input: $error\n";
    exit;
}

addYourself($happinessMap);
$guests = getGuestList($happinessMap);
$maxHappiness = calculateOptimalArrangement($guests, $happinessMap);
echo $maxHappiness . "\n";

?>