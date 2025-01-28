
<?php

function solve() {
    $input = file('input.txt', FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    if ($input === false) {
        die("Error reading input.txt\n");
    }

    $initial_values = [];
    $gates = [];

    // Parse input
    $parsing_initial_values = true;
    foreach ($input as $line) {
        if (strpos($line, '->') === false) {
            if ($parsing_initial_values) {
                list($wire, $value) = explode(': ', $line);
                $initial_values[$wire] = intval($value);
            }
        } else {
            $parsing_initial_values = false;
            $parts = explode(' ', $line);
            $input1 = $parts[0];
            $gate_type = $parts[1];
            $input2 = $parts[2];
            $output_wire = $parts[4];
            $gates[] = [
                'input1' => $input1,
                'input2' => $input2,
                'type' => $gate_type,
                'output' => $output_wire,
            ];
        }
    }

    $wires = $initial_values;
    $changed = true;

    while ($changed) {
        $changed = false;
        foreach ($gates as $gate) {
            $input1_val = isset($wires[$gate['input1']]) ? $wires[$gate['input1']] : null;
            $input2_val = isset($wires[$gate['input2']]) ? $wires[$gate['input2']] : null;

            if ($input1_val !== null && $input2_val !== null) {
                $output_val = null;
                switch ($gate['type']) {
                    case 'AND':
                        $output_val = $input1_val & $input2_val;
                        break;
                    case 'OR':
                        $output_val = $input1_val | $input2_val;
                        break;
                    case 'XOR':
                        $output_val = $input1_val ^ $input2_val;
                        break;
                }

                if ($output_val !== null) {
                    if (!isset($wires[$gate['output']]) || $wires[$gate['output']] !== $output_val) {
                        $wires[$gate['output']] = $output_val;
                        $changed = true;
                    }
                }
            }
        }
    }

    $z_wires = [];
    foreach ($wires as $wire => $value) {
        if (strpos($wire, 'z') === 0) {
            $z_wires[$wire] = $value;
        }
    }

    ksort($z_wires); // Sort z wires by name (z00, z01, z02...)

    $binary_string = '';
    foreach ($z_wires as $wire => $value) {
        $binary_string .= $value;
    }

    if (empty($binary_string)) {
        echo 0 . "\n";
    } else {
        echo bindec(strrev($binary_string)) . "\n"; // Reverse for correct bit order and convert to decimal
    }
}

solve();

?>
