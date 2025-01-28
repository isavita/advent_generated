
<?php

function solveRaces($races) {
    $totalWays = 1;
    foreach ($races as $race) {
        $time = $race['time'];
        $record = $race['distance'];
        $ways = calculateWaysToWin($time, $record);
        $totalWays *= $ways;
    }
    return $totalWays;
}

function calculateWaysToWin($time, $record) {
    $discriminant = $time * $time - 4 * $record;
    if ($discriminant < 0) {
        return 0;
    }
    $sqrtDiscriminant = sqrt($discriminant);
    $h1 = ($time - $sqrtDiscriminant) / 2;
    $h2 = ($time + $sqrtDiscriminant) / 2;

    $start = ceil($h1);
    $end = floor($h2);

    if ($start > $h1) {
        $start = ceil($h1);
    } else {
        $start = ceil($h1 + 1e-9); // Add a small epsilon to handle floating point inaccuracies
    }

    if ($end < $h2) {
        $end = floor($h2);
    } else {
        $end = floor($h2 - 1e-9); // Subtract a small epsilon to handle floating point inaccuracies
    }


    if ($start > $end) {
        return 0;
    } else {
        return max(0, $end - $start + 1);
    }
}

$inputFile = 'input.txt';
$lines = file($inputFile, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

if ($lines === false) {
    die("Error reading input file: $inputFile");
}

$timesLine = $lines[0];
$distancesLine = $lines[1];

preg_match_all('/\d+/', $timesLine, $timesMatches);
preg_match_all('/\d+/', $distancesLine, $distancesMatches);

$times = array_map('intval', $timesMatches[0]);
$distances = array_map('intval', $distancesMatches[0]);

$races = [];
for ($i = 0; $i < count($times); ++$i) {
    $races[] = ['time' => $times[$i], 'distance' => $distances[$i]];
}

$result = solveRaces($races);
echo $result . "\n";

?>
