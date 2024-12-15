
<?php

function parseReactions(string $input): array
{
    $reactions = [];
    foreach (explode("\n", trim($input)) as $line) {
        [$inputs, $output] = explode(" => ", $line);
        preg_match('/(\d+) (\w+)/', $output, $outputMatch);
        $outputQty = (int)$outputMatch[1];
        $outputChem = $outputMatch[2];

        $inputList = [];
        foreach (explode(", ", $inputs) as $input) {
            preg_match('/(\d+) (\w+)/', $input, $inputMatch);
            $inputList[$inputMatch[2]] = (int)$inputMatch[1];
        }
        $reactions[$outputChem] = ['inputs' => $inputList, 'outputQty' => $outputQty];
    }
    return $reactions;
}

function calculateOreForFuel(array $reactions, int $fuelAmount = 1): int
{
    $needed = ['FUEL' => $fuelAmount];
    $oreNeeded = 0;
    $inventory = [];

    while (count($needed) > 0) {
        $chem = array_key_first($needed);
        $qty = $needed[$chem];
        unset($needed[$chem]);

        if ($chem === 'ORE') {
            $oreNeeded += $qty;
            continue;
        }

        if (isset($inventory[$chem]) && $inventory[$chem] >= $qty) {
            $inventory[$chem] -= $qty;
            continue;
        }

        if (isset($inventory[$chem])) {
            $qty -= $inventory[$chem];
            $inventory[$chem] = 0;
        }

        $reaction = $reactions[$chem];
        $multiplier = (int)ceil($qty / $reaction['outputQty']);

        foreach ($reaction['inputs'] as $inputChem => $inputQty) {
            $totalInputQty = $inputQty * $multiplier;
            if (isset($needed[$inputChem])) {
                $needed[$inputChem] += $totalInputQty;
            } else {
                $needed[$inputChem] = $totalInputQty;
            }
        }
        $produced = $reaction['outputQty'] * $multiplier;
        $leftover = $produced - $qty;
        if ($leftover > 0) {
            if (isset($inventory[$chem])) {
                $inventory[$chem] += $leftover;
            } else {
                $inventory[$chem] = $leftover;
            }
        }
    }
    return $oreNeeded;
}

function calculateMaxFuel(array $reactions, int $totalOre): int
{
    $low = 0;
    $high = $totalOre;
    $maxFuel = 0;

    while ($low <= $high) {
        $mid = (int)(($low + $high) / 2);
        $oreNeeded = calculateOreForFuel($reactions, $mid);

        if ($oreNeeded <= $totalOre) {
            $maxFuel = $mid;
            $low = $mid + 1;
        } else {
            $high = $mid - 1;
        }
    }
    return $maxFuel;
}

$input = file_get_contents('input.txt');
$reactions = parseReactions($input);

$oreForOneFuel = calculateOreForFuel($reactions);
echo "Part 1: " . $oreForOneFuel . "\n";

$totalOre = 1000000000000;
$maxFuel = calculateMaxFuel($reactions, $totalOre);
echo "Part 2: " . $maxFuel . "\n";
