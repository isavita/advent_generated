<?php
// Step 1: Read Input
$data = file_get_contents("input.txt");
$banks = array_map('intval', preg_split('/\s+/', trim($data)));

// Step 2: Initialize Variables
$seen = [];
$cycles = 0;

// Step 3: Redistribution Loop
while (true) {
    $state = implode(',', $banks);

    // Step 4: Check for Repeats
    if (isset($seen[$state])) {
        break;
    }
    $seen[$state] = true;

    // Find the bank with most blocks
    $maxIndex = 0;
    $maxValue = $banks[0];
    for ($i = 1; $i < count($banks); $i++) {
        if ($banks[$i] > $maxValue) {
            $maxIndex = $i;
            $maxValue = $banks[$i];
        }
    }

    // Perform redistribution
    $blocks = $banks[$maxIndex];
    $banks[$maxIndex] = 0;
    for ($i = 1; $i <= $blocks; $i++) {
        $banks[($maxIndex + $i) % count($banks)]++;
    }

    // Increment cycle counter
    $cycles++;
}

// Output
echo "It takes $cycles redistribution cycles to reach a repeated configuration.";
?>