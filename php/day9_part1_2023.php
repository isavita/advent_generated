<?php

function parseInput($input): array
{
    $histories = [];
    foreach ($input as $line) {
        $numbers = parseStringToInts($line);
        $histories[] = $numbers;
    }
    return $histories;
}

function parseStringToInts(string $numbersLine): array
{
    $numbers = [];
    $numbersParts = explode(' ', $numbersLine);
    foreach ($numbersParts as $numberStr) {
        $numbers[] = intval($numberStr);
    }
    return $numbers;
}

function allZeros(array $nums): bool
{
    foreach ($nums as $num) {
        if ($num != 0) {
            return false;
        }
    }
    return true;
}

function calculateExtrapolation(array $history): array
{
    $extrapolations = [];
    for ($i = 1; $i < count($history); $i++) {
        $extrapolation = $history[$i] - $history[$i - 1];
        $extrapolations[] = $extrapolation;
    }
    return $extrapolations;
}

function calculateExtrapolations(array $history): array
{
    $extrapolationsSeries = [$history];

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

function solve(array $input): int
{
    $histories = parseInput($input);
    $res = 0;

    foreach ($histories as $history) {
        $extrapolationsSeries = calculateExtrapolations($history);

        $futurePrediction = 0;
        for ($i = count($extrapolationsSeries) - 1; $i >= 0; $i--) {
            $futurePrediction = $extrapolationsSeries[$i][count($extrapolationsSeries[$i]) - 1] + $futurePrediction;
        }

        $res += $futurePrediction;
    }

    return $res;
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES);
echo solve($input) . "\n";