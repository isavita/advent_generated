
<?php

function parseInput($filePath) {
    $lines = file($filePath, FILE_IGNORE_NEW_LINES);
    $initialState = $lines[0][strlen($lines[0])-2];
    $steps = (int)preg_replace('/\D/', '', $lines[1]);

    $states = [];
    for ($i = 3; $i < count($lines); $i += 10) {
        $state = $lines[$i][strlen($lines[$i])-2];
        $value0 = (int)$lines[$i+2][strlen($lines[$i+2])-2];
        $move0 = 1;
        if (strpos($lines[$i+3], "left.") !== false) {
            $move0 = -1;
        }
        $nextState0 = $lines[$i+4][strlen($lines[$i+4])-2];
        $value1 = (int)$lines[$i+6][strlen($lines[$i+6])-2];
        $move1 = 1;
        if (strpos($lines[$i+7], "left.") !== false) {
            $move1 = -1;
        }
        $nextState1 = $lines[$i+8][strlen($lines[$i+8])-2];
        $states[$state] = [0 => [$value0, $move0, $nextState0], 1 => [$value1, $move1, $nextState1]];
    }
    return [$initialState, $steps, $states];
}

function runTuringMachine($filePath) {
    list($state, $steps, $states) = parseInput($filePath);
    $tape = [];
    $cursor = 0;
    $checksum = 0;

    for ($i = 0; $i < $steps; $i++) {
        $value = isset($tape[$cursor]) ? $tape[$cursor] : 0;
        $newValue = $states[$state][$value][0];
        $move = $states[$state][$value][1];
        $nextState = $states[$state][$value][2];

        $tape[$cursor] = $newValue;
        $cursor += $move;
        $state = $nextState;
    }

    foreach ($tape as $v) {
        $checksum += $v;
    }
    return $checksum;
}

$result = runTuringMachine("input.txt");
echo $result . PHP_EOL;

?>
