<?php
$input = file_get_contents("input.txt");
echo solve($input);

function solve($input) {
    $opcodeComputer = parseInput($input);

    while (!tick($opcodeComputer)) {
        if ($opcodeComputer['registers'][$opcodeComputer['instructionPointer']] == 28) {
            break;
        }
    }

    return $opcodeComputer['registers'][5];
}

function parseInput($input) {
    $lines = explode("\n", trim($input));
    sscanf($lines[0], "#ip %d", $instructionPointer);
    $instructions = [];

    for ($i = 1; $i < count($lines); $i++) {
        $parts = explode(" ", $lines[$i]);
        $instructions[] = [
            'name' => $parts[0],
            'abcValues' => [(int)$parts[1], (int)$parts[2], (int)$parts[3]]
        ];
    }

    return [
        'instructions' => $instructions,
        'registers' => [0, 0, 0, 0, 0, 0],
        'instructionPointer' => $instructionPointer
    ];
}

function tick(&$opcodeComputer) {
    $ip = &$opcodeComputer['instructionPointer'];
    $registers = &$opcodeComputer['registers'];
    $instructions = $opcodeComputer['instructions'];

    if ($registers[$ip] >= count($instructions)) {
        return true;
    }

    $inst = $instructions[$registers[$ip]];
    $func = $inst['name'];
    $abc = $inst['abcValues'];

    $registers = opcodeFuncs($func, $registers, $abc);
    $registers[$ip]++;

    if ($registers[$ip] >= count($instructions)) {
        return true;
    }

    return false;
}

function opcodeFuncs($func, $registers, $abc) {
    switch ($func) {
        case 'addr': $registers[$abc[2]] = $registers[$abc[0]] + $registers[$abc[1]]; break;
        case 'addi': $registers[$abc[2]] = $registers[$abc[0]] + $abc[1]; break;
        case 'mulr': $registers[$abc[2]] = $registers[$abc[0]] * $registers[$abc[1]]; break;
        case 'muli': $registers[$abc[2]] = $registers[$abc[0]] * $abc[1]; break;
        case 'banr': $registers[$abc[2]] = $registers[$abc[0]] & $registers[$abc[1]]; break;
        case 'bani': $registers[$abc[2]] = $registers[$abc[0]] & $abc[1]; break;
        case 'borr': $registers[$abc[2]] = $registers[$abc[0]] | $registers[$abc[1]]; break;
        case 'bori': $registers[$abc[2]] = $registers[$abc[0]] | $abc[1]; break;
        case 'setr': $registers[$abc[2]] = $registers[$abc[0]]; break;
        case 'seti': $registers[$abc[2]] = $abc[0]; break;
        case 'gtir': $registers[$abc[2]] = ($abc[0] > $registers[$abc[1]]) ? 1 : 0; break;
        case 'gtri': $registers[$abc[2]] = ($registers[$abc[0]] > $abc[1]) ? 1 : 0; break;
        case 'gtrr': $registers[$abc[2]] = ($registers[$abc[0]] > $registers[$abc[1]]) ? 1 : 0; break;
        case 'eqir': $registers[$abc[2]] = ($abc[0] == $registers[$abc[1]]) ? 1 : 0; break;
        case 'eqri': $registers[$abc[2]] = ($registers[$abc[0]] == $abc[1]) ? 1 : 0; break;
        case 'eqrr': $registers[$abc[2]] = ($registers[$abc[0]] == $registers[$abc[1]]) ? 1 : 0; break;
    }
    return $registers;
}
?>