
<?php

function solve(int $initialA): int {
    $instructions = array_map(fn($line) => explode(' ', trim($line)), file('input.txt'));
    $registers = ['a' => $initialA, 'b' => 0, 'c' => 0, 'd' => 0];
    $ip = 0;

    while ($ip >= 0 && $ip < count($instructions)) {
        $instruction = $instructions[$ip];
        $op = $instruction[0];

        switch ($op) {
            case 'cpy':
                if (is_numeric($instruction[2])) break;
                $registers[$instruction[2]] = is_numeric($instruction[1]) ? (int)$instruction[1] : $registers[$instruction[1]];
                break;
            case 'inc':
                if (is_numeric($instruction[1])) break;
                $registers[$instruction[1]]++;
                break;
            case 'dec':
                if (is_numeric($instruction[1])) break;
                $registers[$instruction[1]]--;
                break;
            case 'jnz':
                $val = is_numeric($instruction[1]) ? (int)$instruction[1] : $registers[$instruction[1]];
                if ($val !== 0) {
                    $ip += (is_numeric($instruction[2]) ? (int)$instruction[2] : $registers[$instruction[2]]) - 1;
                }
                break;
            case 'tgl':
                $target = $ip + (is_numeric($instruction[1]) ? (int)$instruction[1] : $registers[$instruction[1]]);
                if ($target >= 0 && $target < count($instructions)) {
                    $targetOp = $instructions[$target][0];
                    if (count($instructions[$target]) == 2) {
                        $instructions[$target][0] = ($targetOp == 'inc') ? 'dec' : 'inc';
                    } else {
                        $instructions[$target][0] = ($targetOp == 'jnz') ? 'cpy' : 'jnz';
                    }
                }
                break;
        }
        $ip++;
    }

    return $registers['a'];
}

// Part 1
echo "Part 1: " . solve(7) . "\n";

// Part 2 (Optimized based on input analysis)
function optimizedSolve(int $initialA): int {
    $instructions = array_map(fn($line) => explode(' ', trim($line)), file('input.txt'));

    $b = (int)$instructions[19][1];
    $c = (int)$instructions[20][1];

    $result = $initialA;
    for ($i = $initialA; $i > 1; $i--) {
        $result *= ($i - 1);
    }
    
    return $result + ($b * $c);
}

echo "Part 2: " . optimizedSolve(12) . "\n";

?>
