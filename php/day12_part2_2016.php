
<?php

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
$instructions = [];
foreach ($lines as $line) {
    $instructions[] = $line;
}

$registers = ["a" => 0, "b" => 0, "c" => 1, "d" => 0];
executeInstructions($instructions, $registers);

echo $registers["a"] . PHP_EOL;

function executeInstructions($instructions, &$registers) {
    $i = 0;
    $n = count($instructions);
    while ($i < $n) {
        $parts = explode(" ", $instructions[$i]);
        switch ($parts[0]) {
            case "cpy":
                $val = getValue($parts[1], $registers);
                $registers[$parts[2]] = $val;
                $i++;
                break;
            case "inc":
                $registers[$parts[1]]++;
                $i++;
                break;
            case "dec":
                $registers[$parts[1]]--;
                $i++;
                break;
            case "jnz":
                $val = getValue($parts[1], $registers);
                if ($val != 0) {
                    $jump = intval($parts[2]);
                    $i += $jump;
                } else {
                    $i++;
                }
                break;
        }
    }
}

function getValue($s, $registers) {
    if (is_numeric($s)) {
        return intval($s);
    } else {
        return $registers[$s];
    }
}
?>
