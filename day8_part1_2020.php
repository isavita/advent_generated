
<?php

$file = fopen("input.txt", "r");
$instructions = [];

while (!feof($file)) {
    $instructions[] = trim(fgets($file));
}

fclose($file);

list($accumulator, $loopDetected) = executeBootCode($instructions);
echo $accumulator . PHP_EOL;

function executeBootCode($instructions) {
    $accumulator = 0;
    $visited = [];
    $currentInstruction = 0;

    while ($currentInstruction < count($instructions)) {
        if (isset($visited[$currentInstruction])) {
            return [$accumulator, true];
        }

        $visited[$currentInstruction] = true;
        $parts = explode(" ", $instructions[$currentInstruction]);
        $op = $parts[0];
        $arg = intval($parts[1]);

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

    return [$accumulator, false];
}
?>
