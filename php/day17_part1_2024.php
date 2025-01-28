
<?php

// Function to get the value of a combo operand
function getComboOperandValue($operand, $registers) {
    if ($operand >= 0 && $operand <= 3) {
        return $operand;
    } elseif ($operand == 4) {
        return $registers['A'];
    } elseif ($operand == 5) {
        return $registers['B'];
    } elseif ($operand == 6) {
        return $registers['C'];
    } else {
        // Operand 7 is reserved, should not appear in valid programs
        return 0; // Or throw an error, depending on desired error handling
    }
}

// Function to execute the program
function executeProgram($program, &$registers) {
    $instructionPointer = 0;
    $output = [];

    while ($instructionPointer < count($program)) {
        $opcode = intval($program[$instructionPointer]);
        if ($instructionPointer + 1 >= count($program)) {
            break; // Halt if no operand
        }
        $operand = intval($program[$instructionPointer + 1]);

        switch ($opcode) {
            case 0: // adv
                $denominator = 2 ** getComboOperandValue($operand, $registers);
                if ($denominator != 0) { // Avoid division by zero, though unlikely given 2**x
                    $registers['A'] = intdiv($registers['A'], $denominator);
                } else {
                    $registers['A'] = 0; // Handle division by zero if denominator somehow becomes 0
                }
                $instructionPointer += 2;
                break;
            case 1: // bxl
                $registers['B'] = $registers['B'] ^ $operand;
                $instructionPointer += 2;
                break;
            case 2: // bst
                $registers['B'] = getComboOperandValue($operand, $registers) % 8;
                $instructionPointer += 2;
                break;
            case 3: // jnz
                if ($registers['A'] != 0) {
                    $instructionPointer = $operand;
                } else {
                    $instructionPointer += 2;
                }
                break;
            case 4: // bxc
                $registers['B'] = $registers['B'] ^ $registers['C'];
                $instructionPointer += 2;
                break;
            case 5: // out
                $output[] = getComboOperandValue($operand, $registers) % 8;
                $instructionPointer += 2;
                break;
            case 6: // bdv
                $denominator = 2 ** getComboOperandValue($operand, $registers);
                if ($denominator != 0) {
                    $registers['B'] = intdiv($registers['A'], $denominator);
                } else {
                    $registers['B'] = 0;
                }
                $instructionPointer += 2;
                break;
            case 7: // cdv
                $denominator = 2 ** getComboOperandValue($operand, $registers);
                if ($denominator != 0) {
                    $registers['C'] = intdiv($registers['A'], $denominator);
                } else {
                    $registers['C'] = 0;
                }
                $instructionPointer += 2;
                break;
            default:
                // Halt on unknown opcode (though problem states opcodes are 0-7)
                return $output;
        }
    }
    return $output;
}

// Read input from file
$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

// Initialize registers
$registers = [
    'A' => intval(explode(": ", $lines[0])[1]),
    'B' => intval(explode(": ", $lines[1])[1]),
    'C' => intval(explode(": ", $lines[2])[1]),
];

// Parse program
$programLine = $lines[4];
$program = explode(",", explode(": ", $programLine)[1]);

// Execute program and get output
$outputValues = executeProgram($program, $registers);

// Print output as comma-separated string
echo implode(",", $outputValues);
echo "\n";

?>
