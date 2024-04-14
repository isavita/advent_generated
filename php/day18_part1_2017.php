<?php

function getValue($arg, &$registers) {
    if (is_numeric($arg)) {
        return (int)$arg;
    }
    return $registers[$arg] ?? 0;
}

$file = fopen("input.txt", "r");
if (!$file) {
    die("Failed to open input.txt");
}

$instructions = [];
while (($line = fgets($file)) !== false) {
    $instructions[] = preg_split('/\s+/', trim($line));
}
fclose($file);

$registers = [];
$lastSound = 0;

for ($i = 0; $i < count($instructions);) {
    $instruction = $instructions[$i];
    $cmd = $instruction[0];
    $arg1 = $instruction[1];

    switch ($cmd) {
        case 'snd':
            $lastSound = getValue($arg1, $registers);
            break;
        case 'set':
            $registers[$arg1] = getValue($instruction[2], $registers);
            break;
        case 'add':
            $registers[$arg1] += getValue($instruction[2], $registers);
            break;
        case 'mul':
            $registers[$arg1] *= getValue($instruction[2], $registers);
            break;
        case 'mod':
            $registers[$arg1] %= getValue($instruction[2], $registers);
            break;
        case 'rcv':
            if (getValue($arg1, $registers) != 0) {
                echo $lastSound;
                return;
            }
            break;
        case 'jgz':
            if (getValue($arg1, $registers) > 0) {
                $i += getValue($instruction[2], $registers);
                continue 2;
            }
            break;
    }
    $i++;
}

?>