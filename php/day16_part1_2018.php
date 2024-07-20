
<?php

$input = file_get_contents("input.txt");
$lines = array_filter(array_map('trim', explode("\n", $input)));

$opcodes = [
    ['name' => 'addr', 'action' => '+', 'a' => 'r', 'b' => 'r'],
    ['name' => 'addi', 'action' => '+', 'a' => 'r', 'b' => 'v'],
    ['name' => 'mulr', 'action' => '*', 'a' => 'r', 'b' => 'r'],
    ['name' => 'muli', 'action' => '*', 'a' => 'r', 'b' => 'v'],
    ['name' => 'banr', 'action' => '&', 'a' => 'r', 'b' => 'r'],
    ['name' => 'bani', 'action' => '&', 'a' => 'r', 'b' => 'v'],
    ['name' => 'borr', 'action' => '|', 'a' => 'r', 'b' => 'r'],
    ['name' => 'bori', 'action' => '|', 'a' => 'r', 'b' => 'v'],
    ['name' => 'setr', 'action' => 'a', 'a' => 'r', 'b' => 'r'],
    ['name' => 'seti', 'action' => 'a', 'a' => 'v', 'b' => 'r'],
    ['name' => 'gtir', 'action' => '>', 'a' => 'v', 'b' => 'r'],
    ['name' => 'gtri', 'action' => '>', 'a' => 'r', 'b' => 'v'],
    ['name' => 'gtrr', 'action' => '>', 'a' => 'r', 'b' => 'r'],
    ['name' => 'eqir', 'action' => '=', 'a' => 'v', 'b' => 'r'],
    ['name' => 'eqri', 'action' => '=', 'a' => 'r', 'b' => 'v'],
    ['name' => 'eqrr', 'action' => '=', 'a' => 'r', 'b' => 'r'],
];

$sum = 0;
$lineCount = 0;

while ($lineCount < count($lines)) {
    if (isset($lines[$lineCount][0]) && $lines[$lineCount][0] === 'B') {
        preg_match_all('/\d+/', $lines[$lineCount], $registers);
        $registers = array_map('intval', $registers[0]);

        preg_match_all('/\d+/', $lines[$lineCount + 1], $instruction);
        $instruction = array_map('intval', $instruction[0]);

        preg_match_all('/\d+/', $lines[$lineCount + 2], $n);
        $n = array_map('intval', $n[0]);

        $tempSum = testCode($registers, $n, $instruction, $opcodes);

        if ($tempSum >= 3) {
            $sum++;
        }

        $lineCount += 4;
    } else {
        break;
    }
}

echo $sum;

function testCode($registers, $n, $instruction, &$opcodes) {
    $sum = 0;
    foreach ($opcodes as &$op) {
        if (matchRegisters($n, runOp($op, $registers, $instruction))) {
            $op['matchCount'][] = $instruction[0];
            $sum++;
        }
    }
    return $sum;
}

function matchRegisters($r, $c) {
    return $r === $c;
}

function runOp($op, $registers, $instruction) {
    $registerCP = $registers;
    $A = $op['a'] === 'r' ? $registerCP[$instruction[1]] : $instruction[1];
    $B = $op['b'] === 'r' ? $registerCP[$instruction[2]] : $instruction[2];

    switch ($op['action']) {
        case '+':
            $registerCP[$instruction[3]] = $A + $B;
            break;
        case '*':
            $registerCP[$instruction[3]] = $A * $B;
            break;
        case '&':
            $registerCP[$instruction[3]] = $A & $B;
            break;
        case '|':
            $registerCP[$instruction[3]] = $A | $B;
            break;
        case 'a':
            $registerCP[$instruction[3]] = $A;
            break;
        case '>':
            $registerCP[$instruction[3]] = $A > $B ? 1 : 0;
            break;
        case '=':
            $registerCP[$instruction[3]] = $A === $B ? 1 : 0;
            break;
    }
    return $registerCP;
}
