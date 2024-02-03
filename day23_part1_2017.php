
<?php

$file = fopen("input.txt", "r");
if (!$file) {
    die("Unable to open file");
}

$mulCount = 0;
$pointer = 0;
$registers = [];
$instructions = [];

while (!feof($file)) {
    $instructions[] = trim(fgets($file));
}

while ($pointer >= 0 && $pointer < count($instructions)) {
    $parts = explode(" ", $instructions[$pointer]);
    $cmd = $parts[0];
    $x = $parts[1];
    $y = $parts[2];

    $getValue = function ($s) use (&$registers) {
        if (is_numeric($s)) {
            return intval($s);
        }
        return $registers[$s];
    };

    switch ($cmd) {
        case "set":
            $registers[$x] = $getValue($y);
            break;
        case "sub":
            $registers[$x] -= $getValue($y);
            break;
        case "mul":
            $registers[$x] *= $getValue($y);
            $mulCount++;
            break;
        case "jnz":
            if ($getValue($x) != 0) {
                $pointer += $getValue($y) - 1;
            }
            break;
    }
    $pointer++;
}

echo $mulCount . PHP_EOL;

fclose($file);
?>
