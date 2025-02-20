<?php
ini_set('memory_limit', '2G');

// Read input and parse numbers
$input = trim(file_get_contents('input.txt'));
$startingNumbers = explode(',', $input);

// Initialize data structures
$spoken = [];
$lastNumber = 0;
$totalNumbers = count($startingNumbers);

// Process initial numbers
for ($i = 0; $i < $totalNumbers; $i++) {
    $num = (int)$startingNumbers[$i];
    if ($i === $totalNumbers - 1) {
        $lastNumber = $num;
    } else {
        $spoken[$num] = $i + 1; // Store turn number (1-based index)
    }
}

// Run simulation for 30 million turns
$startTurn = $totalNumbers + 1;
for ($turn = $startTurn; $turn <= 30000000; $turn++) {
    // Calculate next number using previous state
    $nextNumber = isset($spoken[$lastNumber]) 
        ? ($turn - 1 - $spoken[$lastNumber]) 
        : 0;

    // Update state for current number before moving to next
    $spoken[$lastNumber] = $turn - 1;
    $lastNumber = $nextNumber;
}

echo $lastNumber . PHP_EOL;
?>
