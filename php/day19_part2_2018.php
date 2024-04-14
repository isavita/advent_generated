<?php

function operation($r, $a, $b, $type) {
    switch ($type) {
        case 'addr': return $r[$a] + $r[$b];
        case 'addi': return $r[$a] + $b;
        case 'mulr': return $r[$a] * $r[$b];
        case 'muli': return $r[$a] * $b;
        case 'banr': return $r[$a] & $r[$b];
        case 'bani': return $r[$a] & $b;
        case 'borr': return $r[$a] | $r[$b];
        case 'bori': return $r[$a] | $b;
        case 'setr': return $r[$a];
        case 'seti': return $a;
        case 'gtir': return $a > $r[$b] ? 1 : 0;
        case 'gtri': return $r[$a] > $b ? 1 : 0;
        case 'gtrr': return $r[$a] > $r[$b] ? 1 : 0;
        case 'eqir': return $a == $r[$b] ? 1 : 0;
        case 'eqri': return $r[$a] == $b ? 1 : 0;
        case 'eqrr': return $r[$a] == $r[$b] ? 1 : 0;
    }
    return 0;
}

function loadProgram($lines) {
    $program = [];
    $ipRegister = 0;
    foreach ($lines as $line) {
        if (strpos($line, "#ip") === 0) {
            $ipRegister = intval(explode(" ", $line)[1]);
        } else {
            preg_match_all('/\d+/', $line, $matches);
            list($a, $b, $c) = $matches[0];
            $parts = explode(" ", $line);
            $op = $parts[0];
            $program[] = function(&$r) use ($op, $a, $b, $c) {
                $r[$c] = operation($r, $a, $b, $op);
            };
        }
    }
    return [$ipRegister, $program];
}

function runProgram($ipRegister, $program, &$registers, $maxCycles) {
    $ip = 0;
    $cycles = 0;
    while ($ip >= 0 && $ip < count($program)) {
        $registers[$ipRegister] = $ip;
        $program[$ip]($registers);
        $ip = $registers[$ipRegister] + 1;
        $cycles++;
        if ($maxCycles > 0 && $cycles >= $maxCycles) {
            break;
        }
    }
    return $registers;
}

function maxArray($array) {
    return max($array);
}

$lines = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
list($ipRegister, $program) = loadProgram($lines);

$registers = array_fill(0, 6, 0);
$registers[0] = 1; // Initialize register 0 to 1 as in Go code
$registers = runProgram($ipRegister, $program, $registers, 1000);
$n = maxArray($registers);
$total = 0;
for ($i = 1; $i <= $n; $i++) {
    if ($n % $i == 0) {
        $total += $i;
    }
}
echo $total;
?>