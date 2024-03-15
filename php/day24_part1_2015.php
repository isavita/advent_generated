<?php
$data = file_get_contents("input.txt");
$lines = explode("\n", trim($data));
$packages = [];
$totalWeight = 0;

foreach ($lines as $line) {
    $weight = intval($line);
    $packages[] = $weight;
    $totalWeight += $weight;
}

$targetWeight = $totalWeight / 3;
$bestQE = PHP_INT_MAX;
$bestLength = PHP_INT_MAX;

// Sort the packages in descending order to optimize the search
sort($packages, SORT_NUMERIC);
$packages = array_reverse($packages);

// Use a recursive function to find the best combination
function findBestCombination($packages, $targetWeight, $currentWeight, $currentQE, $currentLength, &$bestQE, &$bestLength) {
    global $totalWeight;

    // Base case: if the current weight is equal to the target weight, update the best values
    if ($currentWeight == $targetWeight) {
        if ($currentLength < $bestLength || ($currentLength == $bestLength && $currentQE < $bestQE)) {
            $bestLength = $currentLength;
            $bestQE = $currentQE;
        }
        return;
    }

    // Base case: if the current weight is greater than the target weight, return
    if ($currentWeight > $targetWeight) {
        return;
    }

    // Recursive case: try adding each package to the current combination
    for ($i = 0; $i < count($packages); $i++) {
        $package = $packages[$i];
        $newWeight = $currentWeight + $package;
        $newQE = $currentQE * $package;
        $newLength = $currentLength + 1;

        // Prune the search if the new weight is greater than the target weight
        if ($newWeight <= $targetWeight) {
            findBestCombination(array_slice($packages, $i + 1), $targetWeight, $newWeight, $newQE, $newLength, $bestQE, $bestLength);
        }
    }
}

findBestCombination($packages, $targetWeight, 0, 1, 0, $bestQE, $bestLength);

echo $bestQE . "\n";