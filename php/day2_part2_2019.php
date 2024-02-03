
<?php

$input = file_get_contents("input.txt");
$strs = explode(",", trim($input));
$original = array_map('intval', $strs);

for ($noun = 0; $noun <= 99; $noun++) {
    for ($verb = 0; $verb <= 99; $verb++) {
        $memory = $original;
        $memory[1] = $noun;
        $memory[2] = $verb;
        if (execute($memory) == 19690720) {
            echo (100 * $noun + $verb) . "\n";
            return;
        }
    }
}

function execute(&$memory) {
    $i = 0;
    while ($i < count($memory)) {
        switch ($memory[$i]) {
            case 1:
                $memory[$memory[$i + 3]] = $memory[$memory[$i + 1]] + $memory[$memory[$i + 2]];
                break;
            case 2:
                $memory[$memory[$i + 3]] = $memory[$memory[$i + 1]] * $memory[$memory[$i + 2]];
                break;
            case 99:
                return $memory[0];
        }
        $i += 4;
    }
    return $memory[0];
}
