
<?php

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);

$ipBind = intval(explode(" ", $lines[0])[1]);
$instructions = [];
for ($i = 1; $i < count($lines); $i++) {
    $instructions[] = explode(" ", $lines[$i]);
}

$registers = array_fill(0, 6, 0);
for ($ip = 0; $ip < count($instructions); $ip++) {
    $registers[$ipBind] = $ip;
    $inst = $instructions[$ip];
    $opcode = $inst[0];
    $a = intval($inst[1]);
    $b = intval($inst[2]);
    $c = intval($inst[3]);

    switch ($opcode) {
        case "addr":
            $registers[$c] = $registers[$a] + $registers[$b];
            break;
        case "addi":
            $registers[$c] = $registers[$a] + $b;
            break;
        case "mulr":
            $registers[$c] = $registers[$a] * $registers[$b];
            break;
        case "muli":
            $registers[$c] = $registers[$a] * $b;
            break;
        case "banr":
            $registers[$c] = $registers[$a] & $registers[$b];
            break;
        case "bani":
            $registers[$c] = $registers[$a] & $b;
            break;
        case "borr":
            $registers[$c] = $registers[$a] | $registers[$b];
            break;
        case "bori":
            $registers[$c] = $registers[$a] | $b;
            break;
        case "setr":
            $registers[$c] = $registers[$a];
            break;
        case "seti":
            $registers[$c] = $a;
            break;
        case "gtir":
            $registers[$c] = ($a > $registers[$b]) ? 1 : 0;
            break;
        case "gtri":
            $registers[$c] = ($registers[$a] > $b) ? 1 : 0;
            break;
        case "gtrr":
            $registers[$c] = ($registers[$a] > $registers[$b]) ? 1 : 0;
            break;
        case "eqir":
            $registers[$c] = ($a == $registers[$b]) ? 1 : 0;
            break;
        case "eqri":
            $registers[$c] = ($registers[$a] == $b) ? 1 : 0;
            break;
        case "eqrr":
            $registers[$c] = ($registers[$a] == $registers[$b]) ? 1 : 0;
            break;
    }

    $ip = $registers[$ipBind];
    if ($ip < 0 || $ip >= count($instructions)) {
        break;
    }
}

echo $registers[0] . PHP_EOL;
?>
