<?php
// Step 1: Read Input
$input = file_get_contents("input.txt");

// Step 2: Initialize Registers
$registers = [];

// Step 3: Process Instructions
$lines = explode("\n", trim($input));
foreach ($lines as $line) {
    $parts = preg_split('/\s+/', $line);
    $reg = $parts[0];
    $op = $parts[1];
    $amount = (int)$parts[2];
    $condReg = $parts[4];
    $condOp = $parts[5];
    $condVal = (int)$parts[6];

    // Ensure registers are initialized
    if (!isset($registers[$reg])) {
        $registers[$reg] = 0;
    }
    if (!isset($registers[$condReg])) {
        $registers[$condReg] = 0;
    }

    // Check condition
    $cond = false;
    switch ($condOp) {
        case '>':
            $cond = $registers[$condReg] > $condVal;
            break;
        case '>=':
            $cond = $registers[$condReg] >= $condVal;
            break;
        case '<':
            $cond = $registers[$condReg] < $condVal;
            break;
        case '<=':
            $cond = $registers[$condReg] <= $condVal;
            break;
        case '==':
            $cond = $registers[$condReg] == $condVal;
            break;
        case '!=':
            $cond = $registers[$condReg] != $condVal;
            break;
    }

    if ($cond) {
        switch ($op) {
            case 'inc':
                $registers[$reg] += $amount;
                break;
            case 'dec':
                $registers[$reg] -= $amount;
                break;
        }
    }
}

// Step 4: Find Max Value
$maxValue = max($registers);

echo $maxValue;
?>