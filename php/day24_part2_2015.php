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

$targetWeight = $totalWeight / 4; // Assuming the input is such that this division results in an integer
$bestQE = PHP_INT_MAX;
$bestLength = PHP_INT_MAX;

// Sorting packages in descending order can help to reach the target weight quicker
rsort($packages);

// Generate all combinations and find the best quantum entanglement
$packageCount = count($packages);
for ($comb = 1; $comb < (1 << $packageCount); $comb++) {
    $groupWeight = 0;
    $qe = 1;
    $groupLength = 0;
    for ($i = 0; $i < $packageCount; $i++) {
        if (($comb & (1 << $i)) !== 0) {
            $groupWeight += $packages[$i];
            $qe *= $packages[$i];
            $groupLength++;
            if ($groupWeight > $targetWeight) {
                break; // No need to continue if the weight is already exceeded
            }
        }
    }
    if ($groupWeight === $targetWeight && $groupLength <= $bestLength) {
        if ($groupLength < $bestLength || $qe < $bestQE) {
            if (canSplit($packages, $comb, $targetWeight)) {
                $bestLength = $groupLength;
                $bestQE = $qe;
            }
        }
    }
}

echo $bestQE;

function canSplit($packages, $firstGroupComb, $targetWeight) {
    $remainingPackages = [];
    foreach ($packages as $i => $weight) {
        if (($firstGroupComb & (1 << $i)) === 0) {
            $remainingPackages[] = $weight;
        }
    }
    return canFormTwoGroups($remainingPackages, $targetWeight);
}

function canFormTwoGroups($packages, $targetWeight) {
    $n = count($packages);
    for ($comb = 1; $comb < (1 << $n); $comb++) {
        $groupWeight = 0;
        for ($i = 0; $i < $n; $i++) {
            if (($comb & (1 << $i)) !== 0) {
                $groupWeight += $packages[$i];
                if ($groupWeight > $targetWeight) {
                    break; // Stop if weight exceeded
                }
            }
        }
        if ($groupWeight === $targetWeight) {
            return true; // Found a valid group, no need to check further
        }
    }
    return false;
}
?>