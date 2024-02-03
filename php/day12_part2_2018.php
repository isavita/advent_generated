
<?php

$file = fopen("input.txt", "r") or die("Unable to open file!");
$initialState = "";
$rules = [];

while (!feof($file)) {
    $line = trim(fgets($file));
    if (strpos($line, "initial state") !== false) {
        $initialState = explode(": ", $line)[1];
    } elseif (strpos($line, "=>") !== false) {
        $parts = explode(" => ", $line);
        $rules[$parts[0]] = $parts[1][0];
    }
}

$state = [];
for ($i = 0; $i < strlen($initialState); $i++) {
    if ($initialState[$i] === '#') {
        $state[$i] = '#';
    }
}

$previousPattern = "";
$previousSum = 0;
$offset = 0;
for ($generation = 0; $generation < 50000000000; $generation++) {
    $newState = [];
    $minPot = min(array_keys($state)) - 2;
    $maxPot = max(array_keys($state)) + 2;
    for ($i = $minPot; $i <= $maxPot; $i++) {
        $pattern = "";
        for ($j = $i - 2; $j <= $i + 2; $j++) {
            $pattern .= isset($state[$j]) && $state[$j] === '#' ? '#' : '.';
        }
        if (isset($rules[$pattern]) && $rules[$pattern] === '#') {
            $newState[$i] = '#';
        }
    }
    $state = $newState;

    list($currentPattern, $currentSum) = statePattern($state);
    if ($currentPattern === $previousPattern) {
        $offset = $currentSum - $previousSum;
        $remainingGenerations = 50000000000 - $generation - 1;
        $finalSum = $currentSum + $offset * $remainingGenerations;
        echo $finalSum . PHP_EOL;
        break;
    }
    $previousPattern = $currentPattern;
    $previousSum = $currentSum;
}

function statePattern($state) {
    $minPot = min(array_keys($state));
    $maxPot = max(array_keys($state));
    $pattern = "";
    $sum = 0;
    for ($i = $minPot; $i <= $maxPot; $i++) {
        if (isset($state[$i]) && $state[$i] === '#') {
            $pattern .= '#';
            $sum += $i;
        } else {
            $pattern .= '.';
        }
    }
    return [$pattern, $sum];
}

fclose($file);
?>
