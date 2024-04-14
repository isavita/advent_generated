<?php
// Step 1: Read Input
$input = file_get_contents("input.txt");

// Step 2: Initialize Registers
$registers = [];

// Initialize highest value
$highestValue = 0;

// Step 3: Process Instructions
$lines = explode("\n", $input);
foreach ($lines as $line) {
    $parts = preg_split('/\s+/', $line);
    if (count($parts) < 7) continue;
    $reg = $parts[0];
    $op = $parts[1];
    $amount = intval($parts[2]);
    $condReg = $parts[4];
    $condOp = $parts[5];
    $condVal = intval($parts[6]);

    // Check condition
    $cond = false;
    switch ($condOp) {
        case ">":
            $cond = ($registers[$condReg] ?? 0) > $condVal;
            break;
        case ">=":
            $cond = ($registers[$condReg] ?? 0) >= $condVal;
            break;
        case "<":
            $cond = ($registers[$condReg] ?? 0) < $condVal;
            break;
        case "<=":
            $cond = ($registers[$condReg] ?? 0) <= $condVal;
            break;
        case "==":
            $cond = ($registers[$condReg] ?? 0) == $condVal;
            break;
        case "!=":
            $cond = ($registers[$condReg] ?? 0) != $condVal;
            break;
    }

    if ($cond) {
        switch ($op) {
            case "inc":
                $registers[$reg] = ($registers[$reg] ?? 0) + $amount;
                break;
            case "dec":
                $registers[$reg] = ($registers[$reg] ?? 0) - $amount;
                break;
        }

        // Update highest value
        if ($registers[$reg] > $highestValue) {
            $highestValue = $registers[$reg];
        }
    }
}

// Step 4: Print the highest value
echo $highestValue;
?>