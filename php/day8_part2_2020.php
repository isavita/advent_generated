
<?php

$instructions = file("input.txt", FILE_IGNORE_NEW_LINES);

function executeBootCode($instructions) {
    $accumulator = 0;
    $visited = [];
    $currentInstruction = 0;

    while ($currentInstruction < count($instructions)) {
        if (isset($visited[$currentInstruction])) {
            return [$accumulator, false];
        }

        $visited[$currentInstruction] = true;
        list($op, $arg) = parseInstruction($instructions[$currentInstruction]);

        switch ($op) {
            case "acc":
                $accumulator += $arg;
                $currentInstruction++;
                break;
            case "jmp":
                $currentInstruction += $arg;
                break;
            case "nop":
                $currentInstruction++;
                break;
        }
    }

    return [$accumulator, true];
}

function parseInstruction($instruction) {
    $parts = explode(" ", $instruction);
    $op = $parts[0];
    $arg = (int)$parts[1];
    return [$op, $arg];
}

foreach ($instructions as $i => $instruction) {
    list($op, $arg) = parseInstruction($instruction);
    if ($op == "acc") {
        continue;
    }

    $modifiedInstructions = $instructions;
    if ($op == "jmp") {
        $modifiedInstructions[$i] = "nop $arg";
    } else {
        $modifiedInstructions[$i] = "jmp $arg";
    }

    list($accumulator, $terminated) = executeBootCode($modifiedInstructions);
    if ($terminated) {
        echo $accumulator . PHP_EOL;
        break;
    }
}
?>
