<?php

function parseInput($input) {
    $histories = [];
    foreach ($input as $line) {
        $numbers = parseStringToInts($line);
        $histories[] = $numbers;
    }
    return $histories;
}

function parseStringToInts($numbersLine) {
    $numbers = [];
    $numbersParts = preg_split('/\s+/', $numbersLine);
    foreach ($numbersParts as $numberStr) {
        $number = intval($numberStr);
        $numbers[] = $number;
    }
    return $numbers;
}

function allZeros($nums) {
    foreach ($nums as $num) {
        if ($num !== 0) {
            return false;
        }
    }
    return true;
}

function calculateExtrapolation($history) {
    $extrapolations = [];
    for ($i = 1; $i < count($history); $i++) {
        $extrapolation = $history[$i] - $history[$i - 1];
        $extrapolations[] = $extrapolation;
    }
    return $extrapolations;
}

function calculateExtrapolations($history) {
    $extrapolationsSeries = [];
    $extrapolationsSeries[] = $history;

    for ($i = 1; $i < count($history); $i++) {
        $previousExtrapolations = $extrapolationsSeries[$i - 1];
        if (allZeros($previousExtrapolations)) {
            return $extrapolationsSeries;
        }

        $extrapolations = calculateExtrapolation($previousExtrapolations);
        $extrapolationsSeries[] = $extrapolations;
    }

    return $extrapolationsSeries;
}

function solve($input) {
    $histories = parseInput($input);
    $res = 0;

    foreach ($histories as $history) {
        $extrapolationsSeries = calculateExtrapolations($history);

        $pastPrediction = 0;
        for ($i = count($extrapolationsSeries) - 1; $i > -1; $i--) {
            $pastPrediction = $extrapolationsSeries[$i][0] - $pastPrediction;
        }

        $res += $pastPrediction;
    }

    return $res;
}

function readInputFile($fileName) {
    $file = file_get_contents($fileName);
    if ($file === false) {
        throw new Exception("Error reading file");
    }

    return array_filter(explode("\n", trim($file)), 'strlen');
}

$input = readInputFile("input.txt");
echo solve($input);

?>