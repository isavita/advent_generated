
<?php

$instructions = file("input.txt", FILE_IGNORE_NEW_LINES);
$registers = ["a" => 7, "b" => 0, "c" => 0, "d" => 0];

$pc = 0;
while ($pc < count($instructions)) {
    $fields = explode(" ", $instructions[$pc]);
    switch ($fields[0]) {
        case "cpy":
            $x = getValue($fields[1], $registers);
            if (array_key_exists($fields[2], $registers)) {
                $registers[$fields[2]] = $x;
            }
            break;
        case "inc":
            if (array_key_exists($fields[1], $registers)) {
                $registers[$fields[1]]++;
            }
            break;
        case "dec":
            if (array_key_exists($fields[1], $registers)) {
                $registers[$fields[1]]--;
            }
            break;
        case "jnz":
            $x = getValue($fields[1], $registers);
            if ($x != 0) {
                $pc += getValue($fields[2], $registers) - 1;
            }
            break;
        case "tgl":
            $x = getValue($fields[1], $registers);
            $tgt = $pc + $x;
            if ($tgt >= 0 && $tgt < count($instructions)) {
                $instructions[$tgt] = toggleInstruction($instructions[$tgt]);
            }
            break;
    }
    $pc++;
}

echo $registers["a"] . PHP_EOL;

function getValue($s, $registers) {
    if (is_numeric($s)) {
        return intval($s);
    }
    return $registers[$s];
}

function toggleInstruction($instr) {
    $parts = explode(" ", $instr);
    switch ($parts[0]) {
        case "inc":
            $parts[0] = "dec";
            break;
        case "dec":
        case "tgl":
            $parts[0] = "inc";
            break;
        case "jnz":
            $parts[0] = "cpy";
            break;
        case "cpy":
            $parts[0] = "jnz";
            break;
    }
    return implode(" ", $parts);
}
?>
